/* rename pe.groupsize to pe.count, the TNR items using groupsize should
be reconfigured to use an arithmetic expression in the startbit field.
*/

   /* find the extract attribute structure for this item */
   ok = get_ptr_to_item_attribute(&pia, psni, EXTRACT);
   if (ok != OK) goto missing_extract_struct;
   pe = pia->u.pe;
   extract = *pe;
   pe = &extract;
   len = 0;

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
            case 4: pe->count = k;      break;
            default: /* outer loop prohibits this branch*/ break;
            }
         } /* end if */
      } /* end for */
      /* attempt to optimize the extraction */
      ok = w_optimize_extract(pe);
      if (ok != OK) goto extract_optimize_err;
   } /* end if */

   /* determine the size of the data area and flag bad type combinations */
   /*
                        Native item_type
                       -----------------------
       Callers_type   | char   i4    r4    r8
       ---------------|-----------------------
             char     | OK     OK*   err   err
             i4       | err    OK    err   err
             r4       | err    OK+   OK    err
             r8       | err    OK+   OK+   OK

       * - assumed 32-bit aligned, used by w_item_char for xlates
       + - conversion handled by w_item_* routines
   */
   extract_strategy = pe->flags & EXTRACT_TYPE_MASK;
   switch(callers_type | item_type)
   {
   case INT_RETURN:
   case FLOAT_RETURN:
   case DOUBLE_RETURN:
      break;
   case INT_RETURN | FLOAT_RETURN:
      if (callers_type == INT_RETURN) goto extract_data_type_err1;
      break;
   case INT_RETURN | DOUBLE_RETURN:
      if (callers_type == INT_RETURN) goto extract_data_type_err1;
      size = size * 2;
      break;
   case FLOAT_RETURN | DOUBLE_RETURN:
      if (callers_type == FLOAT_RETURN) goto extract_data_type_err1;
      size = size * 2;
      break;
   case CHAR_RETURN:
      extract_strategy = EXTRACT_8_BIT_CH_ARRAY;
      break;
   default:
      if (callers_type == CHAR_RETURN && item_type == INT_RETURN)
      {  /* this is for xlates */
         size = size / 4;
         break;
      }
      if (callers_type == (PCHAR_RETURN | DOUBLE_RETURN))
      {	 /* special for w_generate_aux_args */
         size = size * 2;
         break;
      }
     goto extract_data_type_err1;
     break;
   }
   *ret_type = item_type;

   /* get a pointer to the where the bits to extract are stored */
   switch_bits = 1;
   ebit = 0;
   k = ch+1;
   ok = w_get_usr_struc_addr(&k,&(pe->area),&addr,&byte_size,&is_little_endian);
   if (ok != 1) goto invalid_area;

/*
printf("%s: extract strategy (1) is %0X.\n", rn, extract_strategy);
*/

   /* composite items are done via bit manipulations */
   if (*bit_start != 0)
   {
      if ((item_type | callers_type) != INT_RETURN) goto extract_data_type_err3;
      switch (extract_strategy)
      {
      case EXTRACT_BITS: break;
      case EXTRACT_8_BIT_SCALER:
         if ((*bit_start % 8) == 0) 
            extract_strategy = EXTRACT_8_BIT_SCALER_AC;
         else
         {
            /* "unoptimize", convert bytes back to bits */
            pe->startbit = pe->startbit * 8;
            pe->bitlen   = pe->bitlen * 8;
            pe->offset   = -1;
	    pe->rep      = 1;
            pe->count    = 0;
            extract_strategy = EXTRACT_BITS;
         }
         break;
      default:
         goto extract_data_type_err4;
      }
   }

   /*
      Do the bit, byte, longword, or quadword extraction
   */

/*
printf("%s: %s extract sb,bl,of,re,co: %d %d %d %d %d fl:%X st:%X %d\n",
 rn, item_name,
pe->startbit, pe->bitlen, pe->offset, pe->rep, pe->count,
 pe->flags,
 extract_strategy,
*bit_start);
*/

   switch(extract_strategy)
   {
   case EXTRACT_64_BIT_SCALER:
      pd = (double *) addr;
      pd2 = (double *) a;
      *pd2 = pd[pe->startbit];
      *ret_size = 1;
      break;
   case EXTRACT_32_BIT_SCALER:
      pi = (int *) addr;
      *a = pi[pe->startbit];
      *ret_size = 1;
      break;
   case EXTRACT_32_BIT_ARRAY:
      pi = (int *) addr;
      j = byte_size / 4;
      j = j - pe->startbit;
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > pe->rep) j = pe->rep;
      if (j > size) j = size;
      for (n=0, i=pe->startbit; 
           n < j;
           i++, n++, ++a) *a = pi[i];
      *ret_size = n;
      break;
   case EXTRACT_32_BIT_OFFSETS:
      pi = (int *) addr;
      j = byte_size / 4;
      j = j - pe->startbit;
      i = j % pe->offset;
      j = j / pe->offset;
      if (i > 0) j++;
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > size) j = size;
      for (n=0, i=pe->startbit; 
           n < j;
           i = i + pe->offset, ++n, ++a) *a = pi[i];
      *ret_size = n;
      break;
   case EXTRACT_32_BIT_GROUPS:
      pi = (int *) addr;
      j = byte_size / 4;                  /* # of elements in area */
      j = j - pe->startbit;               /* subtract any prefix */
      i = j % pe->offset;
      j = j / pe->offset;                 /* get number of groups */
      if (i > 0) j++;
      j = j * pe->rep;                    /* multiply by reps per group */
      if (j > size) j = size;             /* compare to caller's buffer sz */
      if (pe->count > 0 && j > pe->count) j = pe->count;
      for (n=0, i=pe->startbit; 
           n < j;
           i = i + pe->offset)
         for (k=i, m=0; 
              m < pe->rep && n < j;
              m++, n++, k++, ++a) *a = pi[k];
      *ret_size = n;
      break;
   case EXTRACT_8_BIT_CH_ARRAY:
      pc = (char *) addr;
      ps = (char *) a;
      j = byte_size;
      j = j - pe->startbit;
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > pe->rep) j = pe->rep;
      if (j > size) j = size;
      for (n=0, pc = &(pc[pe->startbit]);
           n < j;
           n++, ++pc, ++ps) *ps = *pc;
      *ret_size = n;
      break;
   case EXTRACT_8_BIT_SCALER:
      len = 8;
      pu = (unsigned char *) addr;
      *a = pu[pe->startbit];
      *ret_size = 1;
      break;
   case EXTRACT_8_BIT_SCALER_AC:
      len = 8;
      i = *bit_start / 8;
      ps = (char *) addr;
      pc = (char *) a;
      pc[i] = ps[pe->startbit];
      *ret_size = 1;
      break;
   case EXTRACT_8_BIT_ARRAY:
      /* copies 1 byte to 32-bit location */
      pu = (unsigned char *) addr;
      j = byte_size;
      j = j - pe->startbit;
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > pe->rep) j = pe->rep;
      if (j > size) j = size;
      for (n=0, i=pe->startbit; 
           n < j;
           i++, n++, ++a) *a = pu[i];
      *ret_size = n;
      break;
   case EXTRACT_8_BIT_OFFSETS:
      pu = (unsigned char *) addr;
      j = byte_size;
      j = j - pe->startbit;
      i = j % pe->offset;
      j = j / pe->offset;
      if (i > 0) j++;
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > size) j = size;
      for (n=0, i=pe->startbit; 
           n < j;
           i = i + pe->offset, ++n, ++a) *a = pu[i];
      *ret_size = n;
      break;
   case EXTRACT_8_BIT_GROUPS:
      pu = (unsigned char *) addr;
      j = byte_size;                      /* # of elements in area */
      j = j - pe->startbit;               /* subtract any prefix */
      i = j % pe->offset;
      j = j / pe->offset;                 /* get number of groups */
      if (i > 0) j++;
      j = j * pe->rep;                    /* multiply by reps per group */
      if (pe->count > 0 && j > pe->count) j = pe->count;
      if (j > size) j = size;
      for (n=0, i=pe->startbit; 
           n < j;
           i = i + pe->offset)
         for (k=i, m=0; 
              m < pe->rep && n < j;
              m++, n++, k++, ++a) *a = pu[k];
      *ret_size = n;
      break;
   case EXTRACT_BITS:
      /* extract bit-wise */
      sbit = pe->startbit;
      len  = pe->bitlen;
      ebit = (8 * byte_size) - 1;
      switch_bits = is_little_endian;
      obit = sbit;
      /* go to end of data segment of event if no count is specified */
     if (pe->count <= 0)
     {
        j = ebit - sbit + 1;
        if (pe->offset > 0) 
        {
           k = (j / pe->offset) * pe->rep;
           i = j % (pe->offset);
           if (i >= len) 
           {
              j = i / len;
              j = (j > pe->rep) ? pe->rep : j;
              k = k + j;
           }
        }
        else
        {
           k = j / len;
           k = k > pe->rep ? pe->rep : k;
        }
        my_count = k;
     }
     else
        my_count = MY_MAX_COUNT;
/*
printf("%s: %s sbit,len,ebit,obit,*addr,my_count: %d %d %d %d %8Xx %d\n", 
rn,item_name,
sbit,len,ebit,obit,*( (int *) addr),my_count);
      pi = (int *) addr;
printf("...addr[0:3] %X %X %X %X\n", pi[0], pi[1], pi[2], pi[3]);
*/

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
/*
printf("%s: %s simple bitshift yeilds: %d %Xx \n", rn, item_name, *a, *a);
*/
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
                     ui = (uj << (remaining - ilen));
                  else
                     ui = (uj << i);
#endif
#ifdef USE_MACOSX
                  if (switch_bits == 0)
                     ui = (uj << (remaining - ilen));
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
/*
printf("%s: RESULT = %d %Xx, ret_size=%d\n", rn, a[0], a[0], *ret_size);
*/
      break;
   default:
      goto extract_data_type_err2;
      break;
   }
/*
printf("%s: %s value is %d or %8XX retsize=%d\n", rn, item_name, a[0], a[0],
*ret_size);
*/
