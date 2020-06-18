/* item.h - data definitions used to implement the wind/waves item database
*/

#define WID_SEARCH_LIST "WIND_DBMS"

#define EXTRACT		 0x00000001	/* item attribute codes */
#define FIXED_VALUE	 0x00000002
#define COMPOSITE	 0x00000004
#define FILE_NAME	 0x00000008
#define VALIDATION	 0x00000010
#define FUNCTION_NUMBER	 0x00000020
#define COUNT_ITEM	 0x00000080
#define TEXT_ITEM        0x00000100
#define XLATE_PRESENT    0x00000200
#define DESCRIPTION      0x00000400
#define AUTHOR_DATE      0x00000800
#define LOOKUP_TABLE     0x00010000
#define FORMAT           0x00020000
#define PROCEDURE_NUMBER 0x00040000
#define CDF_ITEM         0x00080000

#define INT_RETURN       0x00001000 	/* these bits must match the mask */
#define FLOAT_RETURN     0x00002000
#define DOUBLE_RETURN    0x00004000
#define CHAR_RETURN      0x00008000
#define TYPE_RETURN_MASK 0x0000f000
#define PCHAR_RETURN     0x00100000
#define UR8_CONVERT      0x00200000
/*
#define SHORT_RETURN     0x00400000
*/
#define FLAGS            0xffffffff

#define	in_primary_header	'1'	/* item "area" location keys */
#define	in_secondary_header	'2'
#define	in_tertiary_header	'3'
#define	in_quartinary_header	'4'
#define	in_data			'5'
#define	in_extra_info		'6'
#define in_associated_hk	'8'
#define in_user_ch_struct	'A'
#define in_extract_info  	'B'

/* validation operation codes */
#define NO_OP          0x0		/* error, no operation */
#define EQ_OP          0x1		/* EQ */
#define GE_OP          0x2		/* GE */
#define GT_OP          0x3		/* GT */
#define LE_OP          0x4		/* LE */
#define LT_OP          0x5		/* LT */
#define NE_OP          0x6		/* NE */
#define AND_OP         0x7		/* AND */
#define BETWEEN_OP_INC 0x8		/* BT inclusive */
#define BETWEEN_OP_EXC 0x9		/* BT exclusive */
#define NOT_BETWEEN_OP 0xa		/* NBT */
#define IS_BIT_SET_OP  0xb		/* IBS */
#define OR_OP          0xc		/* OR */
#define DATE_LT_OP     0xd		/* DLT */
#define DATE_LE_OP     0xe		/* DLE */
#define DATE_GT_OP     0xf		/* DGT */
#define DATE_GE_OP     0x10		/* DGE */

#define MAX_AUXILLIARY_ARGS 16
#define MAX_LOOKUP_TBL_ENTRIES 32

#define ITEM_NAME_PARAM_INTRO '$'

struct get_item_arg_blk {
  int  ch             ;	/* user's channel number			*/
  char *event_name    ;	/* character string event type			*/
  char *item_name     ; /* item name as a string			*/
  int  *a             ;	/* user's storage area (buffer)			*/
  int  size           ;	/* size of user's buffer in data type units	*/
  int  *ret_size      ;	/* # of type units written to user's area	*/
  int  *bit_start     ; /* starting bit position for composite items	*/
  int  *ret_type      ; /* data type of [buffer on input, item on ret]	*/
  struct same_name_item **ret_psni; /* for caller's item aux info call  */
};

/* Multi-Data Types */
struct mdt {
   union {
      int i4;
      int ai4[2];
      unsigned int ui4;
      float r4;
      double r8;
      char *pc;
   } u;
};

struct string_list {			/* general purpose linked str list */
   char *str;
   struct string_list *next;
};

typedef struct xlate_item_def {		/* container for XLATE= clauses */
    int val;
    char *str;
    void *next;
} xlate_item;

struct validation {			/* container for VALIDATION= clauses */
   char *name;
   int val1;
   int val2;
   int operation;
};

#define EXTRACT_HAS_EXPR        0x10000000
#define EXTRACT_BITS            0x00000010
#define EXTRACT_8_BIT_CH_ARRAY  0x00000040	/* Character Array   */
#define EXTRACT_8_BIT_SCALER_AC 0x00000080	/* Aligned Composite */
#define EXTRACT_8_BIT_SCALER    0x00000100
#define EXTRACT_8_BIT_ARRAY     0x00000200
#define EXTRACT_8_BIT_OFFSETS   0x00000400
#define EXTRACT_8_BIT_GROUPS    0x00000800
#define EXTRACT_32_BIT_SCALER   0x00001000
#define EXTRACT_32_BIT_ARRAY    0x00002000
#define EXTRACT_32_BIT_OFFSETS  0x00004000
#define EXTRACT_32_BIT_GROUPS   0x00008000
#define EXTRACT_64_BIT_SCALER   0x00010000
#define EXTRACT_64_BIT_ARRAY    0x00020000
#define EXTRACT_64_BIT_OFFSETS  0x00040000
#define EXTRACT_64_BIT_GROUPS   0x00080000
#define EXTRACT_TYPE_MASK       0x000ffff0

struct extract {			/* container for EXTRACT= clause */
   int startbit;
   int bitlen;
   int offset;
   int rep;
   int count; /* max number of elements to extract (not groups) */
/* int group_size; */
   int area;  /* eg., primary header, secondary header, ... */
   int flags; /* extract= specific flags */
   char *expr[5];
};

struct fixed_value {			/* container for FIXED_VALUE= clause */
   int value_count;
   union {
      int *ivals;    /* an array of ints */
      float *fvals;  /* an array of floats */
      double *dvals; /* an array of doubles */
      char *cvals;   /* an array of characters */
   } u;
   char *expr;       /* run-time arthmetic expression */
};

struct function_arg_list {
   char *pc;				/* item name or orig string constant */
   struct function_arg_list *next;
   int data_type;
   union {				/* holds resolved item or constant */
      int ival[2];
      float fval[2];
      double dval;
      char *pc;
   } u;
};

struct function {			/* container for FUNCTION= clause */
   int function_number;
   int arg_count;
   struct function_arg_list *head;
};

struct procedure {		/* container for special function-like items */
   int procedure_number;
};
#define PHYSICAL_UNITS_PROC 1

#define W_CDF_SEARCH_INTERPOLATE 1
#define W_CDF_SEARCH_NEAREST     2
#define W_CDF_SEARCH_EARLIER     3
#define W_CDF_SEARCH_LATER       4
#define MAX_CDF_INDICES 10
#define MAX_CDF_FILES 10

struct cdf_item {               /* container for CDF item clauses */
   char *independent_rv;
   char *independent_rv_match;
   char *dependent_rv;
   int  dependent_rv_indices[MAX_CDF_INDICES+1];
   int  use_dependent_rv_ranges;
   int  dependent_rv_ranges[MAX_CDF_INDICES+1][4];
   int  dependent_rv_range_order[MAX_CDF_INDICES+1];
   int  independent_rv_indices[MAX_CDF_INDICES+1];
   int  search_mode;
   int  search_mode_2nd;
   struct string_list *file_list;
};

struct lookup {
   char *name;			/* name of item to lookup for orig val */
   union {
      int   itbl[MAX_LOOKUP_TBL_ENTRIES];
      float ftbl[MAX_LOOKUP_TBL_ENTRIES];
      double dtbl[MAX_LOOKUP_TBL_ENTRIES];
      char *ctbl[MAX_LOOKUP_TBL_ENTRIES];
   } u;
};

/* Linked list node definition for list of item attributes */
struct item_attributes {
   struct item_attributes *prev;
   struct item_attributes *next;
   int kind_of;
   union {
      struct extract *pe;
      struct fixed_value *pfv;
      struct validation *pv;
      xlate_item *xlate_ptr;
      struct function *pfun;
      struct lookup *plook;
      struct procedure *pproc;
      struct cdf_item *pcdf;
      char   *myfile;			/* FILE= clause */
      char   *chain;			/* COMPOSITE= clause */
      char   *count;			/* COUNT= clause */
      char   *text;			/* TEXT= clause */
      char   *desc;			/* DESC= clause */
      char   *author_date;		/* AUTHOR_DATE= clause */
      char   *format;			/* FORMAT= clause */
   } u;
};

/* Linked list node definition for linked list of same named items. */
struct same_name_item {
   struct same_name_item *prev;
   struct same_name_item *next;
   int flags;
   struct item_attributes *head;
};

/* 
   The names of all items described for an event are stored in a red-black
   binary search tree.
*/
struct item_node {
   struct item_node *left;
   struct item_node *right;
   struct item_node *parent;
   int    color;
   char   *item_name;
   struct same_name_item *list;
};

/*
   Each event name is used to identify a group of items
*/
struct event_node {
   struct event_node *next;
   char *event_name;
   struct item_node *head;
   int n;
   char *source_filename;
};

/*
	The following structures are for FORTRAN callers retrieving item
	attribute information in fixed size buffers.  These structures
	must match definitions given item_def.for.
*/
#define FORTRAN_ITEM_STR_LEN 32

struct fortran_xlate_item {
    char str[FORTRAN_ITEM_STR_LEN];
    int val;
} ;

struct fortran_validation {
   char name[FORTRAN_ITEM_STR_LEN];
   int val1;
   int val2;
   int operation;
};

struct fortran_extract {
   int startbit;
   int length;
   int offset;
   int rep;
   int group_size;
   int area;
   int flags;
   char *pchar[5];
};

#define FORTRAN_MAX_BYTES 256
struct fortran_fixed_value {
   int value_count;
   int data_type;
   union {
      int ivals[FORTRAN_MAX_BYTES/4];    /* an array of ints */
      float fvals[FORTRAN_MAX_BYTES/4];  /* an array of floats */
      double dvals[FORTRAN_MAX_BYTES/8]; /* an array of doubles */
      char cvals[FORTRAN_MAX_BYTES];   /* an array of characters */
   } u;
   char *pchar;
};

struct fortran_function_arg_list {
   char pc[FORTRAN_ITEM_STR_LEN];
   int data_type;
   int function_number;
   union {
      int ival[2];
      float fval[2];
      double dval;
   } u;
};


struct fortran_lookup_table {
   char name[FORTRAN_ITEM_STR_LEN];  /* name of item to lookup for orig val */
   int data_type;
   union {
      int   itbl[MAX_LOOKUP_TBL_ENTRIES];
      float ftbl[MAX_LOOKUP_TBL_ENTRIES];
      double dtb[MAX_LOOKUP_TBL_ENTRIES/2];
      char ctb[FORTRAN_ITEM_STR_LEN][MAX_LOOKUP_TBL_ENTRIES];
   } u;
};


struct pc_or_int {
   union {
      int ival[2];
      char *pc;
   } u;
};

struct fortran_cdf_item {
   char independent_rv[32];
   char independent_rv_match[32];
   char dependent_rv[32];
   int  dependent_rv_indices[MAX_CDF_INDICES+1];
   int  use_dependent_rv_ranges;
   int  dependent_rv_ranges[MAX_CDF_INDICES+1][4];
   int  dependent_rv_range_order[MAX_CDF_INDICES+1];
   int  independent_rv_indices[MAX_CDF_INDICES+1];
   int  search_mode;
   int  search_mode_2nd;
   int  n_files;
   char files[MAX_CDF_FILES][256];
};
