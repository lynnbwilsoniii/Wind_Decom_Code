  /* all other items are of type integer */
  /* size must now be converted into longword units */
  if (*ret_type != INT_RETURN)
  {
     /* adjust data type for longword units */
     switch(*ret_type)
     {
     case DOUBLE_RETURN:
        size = size * 2;
        break;
     case CHAR_RETURN:
        size = size / 4;
        break;
     case PCHAR_RETURN | DOUBLE_RETURN:	/* special for w_generate_aux_args */
        size = size * 2;
        break;
     case INT_RETURN:
     case FLOAT_RETURN:
        break;
     default:
        printf("%s: unknown type for sizing extract.\n", rn);
        size = 1;
        break;
     }
  }
  *ret_type = INT_RETURN;

  /* find the extract attribute structure for this item */
  ok = get_ptr_to_item_attribute(&pia, psni, EXTRACT);
  if (ok != OK) goto missing_extract_struct;
  pe = pia->u.pe;

  /* get a pointer to the where the bits to extract are stored */
  switch_bits = 1;
  ebit = 0;
  k = ch+1;
  ok = w_get_usr_struc_addr(&k,&(pe->area),&addr,&i,&is_little_endian);
  if (ok != 1) goto invalid_area;
  ebit = (8 * i) - 1;
  switch_bits = is_little_endian;

   /* resolve any embedded run time arithmetic expressions */
   if ((pe->flags & EXTRACT_HAS_EXPR) != 0)
   {
      for (j=0; j < 5; ++j)
      {
         if (pe->expr[j] != NULL)
         {
/*
printf("%s: ...calling to evaluate extract expression for %s...\n", rn, item_name);
printf("%s: ...expr=%s\n", rn, pe->expr[j]);
*/
            ok = w_item_expr_eval(ch, event_name, item_name, 
                 pe->expr[j],
                 &xmdt, &i,
                 w_item_expr_var_resolve, wc_msg);
            if (ok != OK) goto extract_eval_err;
            if (i == INT_RETURN) {k = xmdt.u.i4;}
            if (i == DOUBLE_RETURN) {k = xmdt.u.r8;}
            switch(j)
            {
            case 0: pe->startbit = k;   break;
            case 1: pe->bitlen = k;     break;
            case 2: pe->offset = k;     break;
            case 3: pe->rep = k;        break;
            case 4: pe->group_size = k; break;
            default: /* outer loop prohibits this branch*/ break;
            }
         } /* end if */
      } /* end for */
   } /* end if */


  /* check for presence of a count item */
  if ((psni->flags & COUNT_ITEM) != 0)
  {
     ok = get_ptr_to_item_attribute(&pia, psni, COUNT_ITEM);
     if (ok != OK) goto missing_extract_struct;
     my_count = 0;
     my_ret_size = 0;
     my_bit_start = 0;
     my_ret_type = INT_RETURN;
#ifdef USE_MACOSX
     ok = w_really_get_item_(
#else
     ok = w_really_get_item(
#endif
		ch,
		event_name,
	        pia->u.count,
		&my_count,
		1,
		&my_ret_size,
		&my_bit_start,
                &my_ret_type,
		&psni2);
    if (ok != OK) goto bad_count;
  }
  else
  {
    my_count = my_max_count;		/* go to end of data segment of event */
  }


  /* extract the item from the telemetry */
  sbit = pe->startbit;
  len  = pe->bitlen;
  /* The count= item specified in the database indicates the number of
     values, or the number of groups of values, to find for a given item.
     When the count= value and "repetitions" value are greater than zero
     we know we are in a group extraction situation, ie, AABBBBC.AABBBBC...,
     where items A, B, and C are each 1 byte long and we want to pick out
     the array of B values occuring in groups of four.
     The count= value is multiplied by the repetitions (repetitions
     are sequential adjacently occuring bit fields) to determine the total
     number of array elements to be extracted.
  */
  group_count = my_count;
  if ((my_count > 0)            &&
      (my_count < my_max_count) &&
      (pe->rep > 1)             &&
      (pe->group_size <= 0))
  {
     my_count = my_count * pe->rep;
  }

  /* In some cases a telemetry item occurs at the end of some number of
     groups, eg. AABC.AABC.AABC.DDE.DDE where the D's and E's are to be
     picked out.  In this case the extract= field is coded such that the
     start bit is a positive or negative offset relative to the end
     of the last group of AABC and the 5th extract= field is the prev group bit
     size.  So, to determine the actual start bit of the item, D, in the
     TM area the prev_group_bit_size is multiplied by the group_value_count
     (from count=) and added to the start_bit value coded in extract= field
     number one.
  */
  if (pe->group_size > 0)
  {
     j = pe->group_size * group_count;
     sbit = j + pe->startbit;
     my_count = my_max_count;		/* go to end of data segment of event */
  }

/*
printf("groupsz=%d, groupcnt=%d, entry.sbit=%d, sbit=%d, ebit=%d\n",
pe->group_size,
group_count,
pe->startbit,
sbit,
ebit);
*/

   /*
      Do the bit (or byte) extraction
   */

   /* extract byte-wise when possible for speed */
   if (len==8 && 
       *bit_start==0 && 
       (sbit%8)==0 && 
       (pe->offset%8)==0 &&
       pe->group_size==0)
   {
      end_byte = (ebit / 8);		/* assume all boundries byte aligned */
      start_byte = (pe->startbit / 8);
      byte_offset = (pe->offset / 8);
      pi = a;
      pu = (unsigned char *) addr;
      i = 0;
      if (pe->rep == 0)
         while (*ret_size<size && start_byte <= end_byte)
         {
            pi[i] = pu[start_byte];
            start_byte = start_byte + byte_offset;
            ++i;
            ++*ret_size;
         }
      else
         while (*ret_size<size && start_byte <= end_byte)
         {
            for(j=0, k=start_byte; j < pe->rep; ++j)
            {
               pi[i++] = pu[k++];
               ++*ret_size;
            }
            start_byte = start_byte + byte_offset;
         }
   }
   else
   {
      /* extract bit-wise */
      obit = sbit;
      while
      ((*ret_size<size) && (sbit>=0) && (sbit<=ebit) && (*ret_size<my_count))
      {
         for (j=0;
             ((*ret_size < size) &&
             (j < pe->rep) &&
             (*ret_size < my_count) &&
             (sbit<=ebit) );
             ++j, ++*ret_size)
         {

         switch(bit_extract_method)
         {
         case 1:
#if SYSTEM == VMS
            ok = cp_bits_to_lw(addr,&sbit,&len, &a[*ret_size], bit_start);
#endif
#if SYSTEM == SUNOS
            ok = cpy_bits2(addr,sbit,len, &a[*ret_size], 
                     *bit_start, switch_bits);
#endif
#ifdef USE_MACOSX
            ok = cpy_bits2(addr,sbit,len, &a[*ret_size], 
                     *bit_start, switch_bits);
#endif
            if (ok!=1) goto bad_conversion;
            break;
         case 2:
            start_byte = sbit / 8;
            pu = (unsigned char *) addr;
            pu = &pu[start_byte];
            ibit = sbit - (start_byte * 8);
            /* simplest case is bit fields no bigger than a byte occuring
               completely within a single byte domain, and the destination
               bit start address is zero (this works for either endian)
            */
            if (*bit_start==0 && (ibit+len) <= 8)
            {
               ui = (unsigned int) *pu;
               a[*ret_size] = (ui >> ibit) & ~(~0 << len);
               break;
            }
            /* remaining case is bit fields straddling byte and/or 32-bit
               boundaries, where the destination bit start address is not
               zero, and endian ordering matters
            */
            for (um=0, remaining=len, i = *bit_start;
                 remaining > 0;
                 pu++)
            {
               ilen = 8 - ibit;
               if (ilen > remaining) ilen = remaining;
               ui = (unsigned int) *pu;
               uj = (ui >> ibit) & ~(~0 << ilen);
#if SYSTEM == VMS
               ui = (uj << i);
#endif
#if SYSTEM == SUNOS
               if (switch_bits == 0)
               {
                  ui = (uj << (remaining - ilen));
               }
               else
                  ui = (uj << i);
#endif
#ifdef USE_MACOSX
               if (switch_bits == 0)
               {
                  ui = (uj << (remaining - ilen));
               }
               else
                  ui = (uj << i);
#endif
               um = um | ui;
               i = i + ilen;
               ibit = (ibit + ilen) % 8;
               remaining = remaining - ilen;
            }
            a[*ret_size] = a[*ret_size] | um;
            break;
         default:
            break;
         } /* switch */
            sbit = sbit + len;
         } /* for */
         if (sbit < ebit) sbit = (pe->offset < 0) ? -1 : obit + pe->offset;
         obit = sbit;
      }
   }

