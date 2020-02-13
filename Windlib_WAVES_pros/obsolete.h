/*
 * obsolete.h - Old API re-written in terms of newer API for compatability.
 *
 *	$Id: obsolete.h,v 1.23 2000/02/24 00:55:07 ali Exp $
 */

/*
  Copyright (c) 1994-2000, Research Systems Inc.  All rights reserved.
  This software includes information which is proprietary to and a
  trade secret of Research Systems, Inc.  It is not to be disclosed
  to anyone outside of this organization. Reproduction by any means
  whatsoever is  prohibited without express written permission.
  */


#ifndef obsolete_IDL_DEF
#define obsolete_IDL_DEF

/*
 * These symbols are variables, functions with varargs style argument lists,
 * or preprocessor symbols whose names have been changed.
 *
 * Programs written in C can use these preprocessor definitions to
 * help compile older code. Non-C programs will have to be translated
 * by hand, as the C preprocessor cannot help them.
 */
#ifndef ABS
#define ABS(x) (((x) >= 0) ? (x) : -(x))
#endif
#define ALLTYPES		IDL_ALLTYPES
#define ARRAY			IDL_ARRAY
#define ATTR_STRUCT		IDL_ATTR_STRUCT
#define ATTR_STRUCT		IDL_ATTR_STRUCT
#define AXIS			IDL_AXIS
#define AXIS			IDL_AXIS
#define AX_EXACT		IDL_AX_EXACT
#define AX_EXTEND		IDL_AX_EXTEND
#define AX_LOG			IDL_AX_LOG
#define AX_MAP			IDL_AX_MAP
#define AX_NOBOX		IDL_AX_NOBOX
#define AX_NONE			IDL_AX_NONE
#define AX_NOZER		IDL_AX_NOZERO
#define A_FILE			IDL_A_FILE
#define A_NO_GUARD		IDL_A_NO_GUARD
#define BASICARR_INI_INDEX	IDL_ARR_INI_INDEX
#define BASICARR_INI_NOP	IDL_ARR_INI_NOP
#define BASICARR_INI_TEST	IDL_ARR_INI_TEST
#define BASICARR_INI_ZERO	IDL_ARR_INI_ZERO
#define CHAR			IDL_CHAR
#define CHARA			IDL_CHARA
#define CLIP_TO_RANGE		IDL_CLIP_TO_RANGE
#define color_map		IDL_ColorMap
#define COLOR_MAP_SIZE		IDL_COLOR_MAP_SIZE
#define COMPLEX			IDL_COMPLEX
#define COORD_DATA		IDL_COORD_DATA
#define COORD_DEVICE		IDL_COORD_DEVICE
#define COORD_IDEVICE		IDL_COORD_IDEVICE
#define COORD_MARGIN		IDL_COORD_MARGIN
#define COORD_NORMAL		IDL_COORD_NORMAL
#define CURS_HIDE		IDL_CURS_HIDE
#define CURS_HIDE_ORIGINAL	IDL_CURS_HIDE_ORIGINAL
#define CURS_RD			IDL_CURS_RD
#define CURS_RD_BUTTON_DOWN	IDL_CURS_RD_BUTTON_DOWN
#define CURS_RD_BUTTON_UP	IDL_CURS_RD_BUTTON_UP
#define CURS_RD_MOVE		IDL_CURS_RD_MOVE
#define CURS_RD_WAIT		IDL_CURS_RD_WAIT
#define CURS_SET		IDL_CURS_SET
#define CURS_SHOW		IDL_CURS_SHOW
#define DCOMPLEX		IDL_DCOMPLEX
#define DELTMP			IDL_DELTMP
#define DELVAR			IDL_DELVAR
#define DEVICE_CORE		IDL_DEVICE_CORE
#define DEVICE_CORE		IDL_DEVICE_CORE
#define DEVICE_DEF		IDL_DEVICE_DEF
#define DEVICE_DEF		IDL_DEVICE_DEF
#define DEVICE_WINDOW		IDL_DEVICE_WINDOW
#define DEVICE_WINDOW		IDL_DEVICE_WINDOW
#define DITHER_FLOYD_STEINBERG	IDL_DITHER_FLOYD_STEINBERG
#define DITHER_F_WHITE		IDL_DITHER_F_WHITE
#define dither_method_names	IDL_DitherMethodNames
#define DITHER_ORDERED		IDL_DITHER_ORDERED
#define DITHER_REVERSE		IDL_DITHER_REVERSE
#define DITHER_THRESHOLD	IDL_DITHER_THRESHOLD
#define D_ANGLE_TEXT		IDL_D_ANGLE_TEXT
#define D_COLOR			IDL_D_COLOR
#define D_HERSH_CONTROL		IDL_D_HERSH_CONTROL
#define D_IMAGE			IDL_D_IMAGE
#define D_KANJI			IDL_D_KANJI
#define D_MONOSPACE		IDL_D_MONOSPACE
#define D_NO_HDW_TEXT		IDL_D_NO_HDW_TEXT
#define D_PLOTTER		IDL_D_PLOTTER
#define D_POLYFILL		IDL_D_POLYFILL
#define D_POLYFILL_LINE		IDL_D_POLYFILL_LINE
#define D_READ_PIXELS		IDL_D_READ_PIXELS
#define D_SCALABLE_PIXELS	IDL_D_SCALABLE_PIXELS
#define D_THICK			IDL_D_THICK
#define D_WHITE_BACKGROUND	IDL_D_WHITE_BACKGROUND
#define D_WIDGETS		IDL_D_WIDGETS
#define D_WINDOWS		IDL_D_WINDOWS
#define D_WORDS			IDL_D_WORDS
#define D_Z			IDL_D_Z
#define EFS_ASSOC		IDL_EFS_ASSOC
#define EFS_CLOSED		IDL_EFS_CLOSED
#define EFS_NOGUI		IDL_EFS_NOGUI
#define EFS_NOPIPE		IDL_EFS_NOPIPE
#define EFS_NOTTY		IDL_EFS_NOTTY
#define EFS_NOT_NOSTDIO		IDL_EFS_NOT_NOSTDIO
#define EFS_NOXDR		IDL_EFS_NOXDR
#define EFS_OPEN		IDL_EFS_OPEN
#define EFS_READ		IDL_EFS_READ
#define EFS_USER		IDL_EFS_USER
#define EFS_WRITE		IDL_EFS_WRITE
#define ENSURE_ARRAY		IDL_ENSURE_ARRAY
#define ENSURE_SCALAR		IDL_ENSURE_SCALAR
#define ENSURE_SIMPLE		IDL_ENSURE_SIMPLE
#define ENSURE_STRING		IDL_ENSURE_STRING
#define ENSURE_STRUCTURE	IDL_ENSURE_STRUCTURE
#define error_code		IDL_SysvErrorCode
#define err_code		IDL_SysvErrCode
#define EXCLUDE_COMPLEX		IDL_EXCLUDE_COMPLEX
#define EXCLUDE_CONST		IDL_EXCLUDE_CONST
#define EXCLUDE_EXPR		IDL_EXCLUDE_EXPR
#define EXCLUDE_FILE		IDL_EXCLUDE_FILE
#define EXCLUDE_SCALAR		IDL_EXCLUDE_SCALAR
#define EXCLUDE_STRING		IDL_EXCLUDE_STRING
#define EXCLUDE_STRUCT		IDL_EXCLUDE_STRUCT
#define EXCLUDE_UNDEF		IDL_EXCLUDE_UNDEF
#define EZ_ACCESS_R		IDL_EZ_ACCESS_R
#define EZ_ACCESS_RW		IDL_EZ_ACCESS_RW
#define EZ_ACCESS_W		IDL_EZ_ACCESS_W
#define EZ_ARG			IDL_EZ_ARG
#define EZ_DIM_ANY		IDL_EZ_DIM_ANY
#define EZ_DIM_ARRAY		IDL_EZ_DIM_ARRAY
#define EZ_DIM_MASK		IDL_EZ_DIM_MASK
#define EZ_POST_TRANSPOSE	IDL_EZ_POST_TRANSPOSE
#define EZ_POST_WRITEBACK	IDL_EZ_POST_WRITEBACK
#define EZ_PRE_SQMATRIX		IDL_EZ_PRE_SQMATRIX
#define EZ_PRE_TRANSPOSE	IDL_EZ_PRE_TRANSPOSE
#define FILE_CLOSE		IDL_FILE_CLOSE
#define FILE_NOCLOSE		IDL_FILE_NOCLOSE
#define FILE_STAT		IDL_FILE_STAT
#define FUN_RET			IDL_FUN_RET
#define F_DEL_ON_CLOSE		IDL_F_DEL_ON_CLOSE
#define F_DOS_BINARY		IDL_F_DOS_BINARY
#define F_ISAGUI		IDL_F_ISAGUI
#define F_ISATTY		IDL_F_ISATTY
#define F_MORE			IDL_F_MORE
#define F_NOCLOSE		IDL_F_NOCLOSE
#define F_SR			IDL_F_SR
#define F_UNIX_F77		IDL_F_UNIX_F77
#define F_UNIX_NOSTDIO		IDL_F_UNIX_NOSTDIO
#define F_UNIX_PIPE		IDL_F_UNIX_PIPE
#define F_UNIX_SPECIAL		IDL_F_UNIX_SPECIAL
#define F_VMS_CCFORTRAN		IDL_F_VMS_CCFORTRAN
#define F_VMS_CCLIST		IDL_F_VMS_CCLIST
#define F_VMS_CCNONE		IDL_F_VMS_CCNONE
#define F_VMS_FIXED		IDL_F_VMS_FIXED
#define F_VMS_INDEXED		IDL_F_VMS_INDEXED
#define F_VMS_PRINT		IDL_F_VMS_PRINT
#define F_VMS_RMSBLK		IDL_F_VMS_RMSBLK
#define F_VMS_RMSBLKUDF		IDL_F_VMS_RMSBLKUDF
#define F_VMS_SEGMENTED		IDL_F_VMS_SEGMENTED
#define F_VMS_SHARED		IDL_F_VMS_SHARED
#define F_VMS_STREAM		IDL_F_VMS_STREAM
#define F_VMS_STREAM_STRICT	IDL_F_VMS_STREAM_STRICT
#define F_VMS_SUBMIT		IDL_F_VMS_SUBMIT
#define F_VMS_SUPERSEDE		IDL_F_VMS_SUPERSEDE
#define F_VMS_TRCLOSE		IDL_F_VMS_TRCLOSE
#define F_VMS_VARIABLE		IDL_F_VMS_VARIABLE
#define F_XDR			IDL_F_XDR
#define GR_PT			IDL_GR_PT
#define IDENT			IDL_IDENT
#ifndef IDL_BARR_INI_INDEX
#define IDL_BARR_INI_INDEX IDL_ARR_INI_INDEX
#endif
#ifndef IDL_BARR_INI_NOP
#define IDL_BARR_INI_NOP IDL_ARR_INI_NOP
#endif
#ifndef IDL_BARR_INI_TEST
#define IDL_BARR_INI_TEST IDL_ARR_INI_TEST
#endif
#ifndef IDL_BARR_INI_ZERO
#define IDL_BARR_INI_ZERO IDL_ARR_INI_ZERO
#endif
#define INTA			IDL_INTA
#define KW_ARGS			IDL_KW_ARGS
#define KW_ARRAY		IDL_KW_ARRAY
#define KW_ARRAY_DESC		IDL_KW_ARR_DESC
#define KW_CLEAN		IDL_KW_CLEAN
#define KW_CLEAN_ALL		IDL_KW_CLEAN_ALL
#define KW_FAST_SCAN		IDL_KW_FAST_SCAN
#define KW_MARK			IDL_KW_MARK
#define KW_OUT			IDL_KW_OUT
#define KW_PAR			IDL_KW_PAR
#define KW_VALUE		IDL_KW_VALUE
#define KW_VALUE_VALUE		IDL_KW_VALUE_MASK
#define KW_VIN			IDL_KW_VIN
#define KW_ZERO			IDL_KW_ZERO
#define LONG			IDL_LONG
#define LONGA			IDL_LONGA
#ifndef MAX
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#endif
#define MAXPARAMS		IDL_MAXPARAMS
#define MAX_ARRAY_DIM		IDL_MAX_ARRAY_DIM
#define MAX_PATH_LEN		IDL_MAX_PATH_LEN
#define MAX_TICKN		IDL_MAX_TICKN
#define MAX_TYPE		IDL_MAX_TYPE
#define message			IDL_MessageErrno
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#define MOUSE_STRUCT		IDL_MOUSE_STRUCT
#define MOUSE_STRUCT		IDL_MOUSE_STRUCT
#define MSG_ACTION_ATTR		IDL_MSG_ACTION_ATTR
#define MSG_ACTION_CODE		IDL_MSG_ACTION_CODE
#define MSG_ATTR_BELL		IDL_MSG_ATTR_BELL
#define MSG_ATTR_MORE		IDL_MSG_ATTR_MORE
#define MSG_ATTR_NOPREFIX	IDL_MSG_ATTR_NOPREFIX
#define MSG_ATTR_NOPRINT	IDL_MSG_ATTR_NOPRINT
#define MSG_ATTR_NOTRACE	IDL_MSG_ATTR_NOTRACE
#define MSG_ATTR_QUIET		IDL_MSG_ATTR_QUIET
#define MSG_ATTR_SYS		IDL_MSG_ATTR_SYS
#define MSG_EXIT		IDL_MSG_EXIT
#define MSG_INFO		IDL_MSG_INFO
#define MSG_IO_LONGJMP		IDL_MSG_IO_LONGJMP
#define MSG_LONGJMP		IDL_MSG_LONGJMP
#define MSG_RET			IDL_MSG_RET
#define M_GENERIC		IDL_M_GENERIC
#define M_NAMED_GENERIC		IDL_M_NAMED_GENERIC
#define M_NOCOMPLEX		IDL_M_NOCOMPLEX
#define M_NOCONST		IDL_M_NOCONST
#define M_NOEXPR		IDL_M_NOEXPR
#define M_NOFILE		IDL_M_NOFILE
#define M_NOSTRING		IDL_M_NOSTRING
#define M_NOSTRUCT		IDL_M_NOSTRUCT
#define M_NOTARRAY		IDL_M_NOTARRAY
#define M_NOTSCALAR		IDL_M_NOTSCALAR
#define M_REQSTR		IDL_M_REQSTR
#define M_SIMVARONLY		IDL_M_SIMVARONLY
#define M_STRUC_REQ		IDL_M_STRUC_REQ
#define M_SYSERR		IDL_M_SYSERR
#define M_UNDEFVAR		IDL_M_UNDEFVAR
#define NON_UNIT		IDL_NON_UNIT
#define NUM_LINESTYLES		IDL_NUM_LINESTYLES
#define NUM_TYPES		IDL_NUM_TYPES
#define oform			IDL_OutputFormat
#define oform_len		IDL_OutputFormatLen
#define OPEN_APND		IDL_OPEN_APND
#define OPEN_NEW		IDL_OPEN_NEW
#define OPEN_R			IDL_OPEN_R
#define OPEN_W			IDL_OPEN_W
#define OS_HAS_TTYS		IDL_OS_HAS_TTYS
#define PH			IDL_PH
#define PLOT_COM		IDL_PLOT_COM
#define PLOT_COM		IDL_PLOT_COM
#define plot_com		IDL_PlotCom
#define POLYFILL_ATTR		IDL_POLYFILL_ATTR
#define POLYFILL_ATTR		IDL_POLYFILL_ATTR
#define pout			IDL_Pout
#define POUT_CNTRL		IDL_POUT_CNTRL
#define POUT_FL			IDL_POUT_FL
#define POUT_GET_POS		IDL_POUT_GET_POS
#define POUT_LEADING		IDL_POUT_LEADING
#define POUT_NOBREAK		IDL_POUT_NOBREAK
#define POUT_NOSP		IDL_POUT_NOSP
#define POUT_SET_POS		IDL_POUT_SET_POS
#define POUT_SL			IDL_POUT_SL
#define PROBLK			IDL_PROBLK
#define program_name		IDL_ProgramName
#define program_name_lc		IDL_ProgramNameLC
#define PRO_ACTIVE		IDL_PRO_ACTIVE
#define PRO_EXTRA		IDL_PRO_EXTRA
#define PRO_FUNFLG		IDL_PRO_FUNFLG
#define PRO_KEYWORDS		IDL_PRO_KEYWORDS
#define PRO_PTR			IDL_PRO_PTR
#define PX			IDL_PX
#define PY			IDL_PY
#define PZ			IDL_PZ
#define RASTER_DEF		IDL_RASTER_DEF
#define RASTER_MSB_LEFT		IDL_RASTER_MSB_LEFT
#define RASTER_MSB_RIGHT	IDL_RASTER_MSB_RIGHT
#define REGISTER		IDL_REGISTER
#define ROUND_UP		IDL_ROUND_UP
#define SHORTA			IDL_SHORTA
#define SREF			IDL_SREF
#define STDERR_UNIT		IDL_STDERR_UNIT
#define STDIN_UNIT		IDL_STDIN_UNIT
#define STDOUT_UNIT		IDL_STDOUT_UNIT
#define STRING			IDL_STRING
#define STRING_STR		IDL_STRING_STR
#define syserror_codes		IDL_SysvSyserrorCodes
#define SYSFUN_DEF		IDL_SYSFUN_DEF
#define SYSVARDEF		IDL_SYSVARDEF
#define sysv_dir		IDL_SysvDir
#define sysv_version		IDL_SysvVersion
#define sys_order		IDL_SysvOrder
#define SYS_VERSION		IDL_SYS_VERSION
#define S_SAVED			IDL_S_SAVED
#define tapechl			IDL_TapeChl
#define term			IDL_FileTerm
#define TERMINFO		IDL_TERMINFO
#define TEXT_STRUCT		IDL_TEXT_STRUCT
#define TEXT_STRUCT		IDL_TEXT_STRUCT
#define TV_STRUCT		IDL_TV_STRUCT
#define type_name		IDL_TypeName
#define type_size		IDL_type_size
#define TYP_BYTE		IDL_TYP_BYTE
#define TYP_B_ALL		IDL_TYP_B_ALL
#define TYP_B_SIMPLE		IDL_TYP_B_SIMPLE
#define TYP_COMPLEX		IDL_TYP_COMPLEX
#define TYP_DCOMPLEX		IDL_TYP_DCOMPLEX
#define TYP_DOUBLE		IDL_TYP_DOUBLE
#define TYP_FLOAT		IDL_TYP_FLOAT
#define TYP_INT			IDL_TYP_INT
#define TYP_LONG		IDL_TYP_LONG
#define TYP_MASK		IDL_TYP_MASK
#define TYP_NUMERIC		IDL_EZ_TYP_NUMERIC
#define TYP_STRING		IDL_TYP_STRING
#define TYP_STRUCT		IDL_TYP_STRUCT
#define TYP_UNDEF		IDL_TYP_UNDEF
#define UCHARA			IDL_UCHARA
#define ULONG			IDL_ULONG
#define ur_err_string		IDL_SysvErrString
#define ur_syserr_string	IDL_SysvSyserrString
#define USER_INFO		IDL_USER_INFO
#define VARIABLE		IDL_VARIABLE
#define vms_message		IDL_MessageVMS
#define VPTR			IDL_VPTR
#define V_ARR			IDL_V_ARR
#define V_CONST			IDL_V_CONST
#define V_DYNAMIC		IDL_V_DYNAMIC
#define V_FILE			IDL_V_FILE
#define V_NOT_SCALAR		IDL_V_NOT_SCALAR
#define V_STRUCT		IDL_V_STRUCT
#define V_TEMP			IDL_V_TEMP
#define WIDGET_STUB_SET_SIZE_FUNC	IDL_WIDGET_STUB_SET_SIZE_FUNC
#define X0			IDL_X0
#define X1			IDL_X1
#define Y0			IDL_Y0
#define Y1			IDL_Y1
#define Z0			IDL_Z0
#define Z1			IDL_Z1



/*
 * Codes for the action parameter to ur_main(). Note that values used
 * cannot be changed without adjusting the code in obsolete.c.
 */
#define MAIN_NORMAL		1
#define MAIN_EXECUTE		2
#define MAIN_EXIT		3
#define MAIN_NOQUIT		4




/* Forward declarations for all obsolete routines emulated in code. */

extern int add_system_routine IDL_ARG_PROTO((IDL_SYSFUN_DEF *defs,
					     int is_function, int cnt));
extern int bail_out IDL_ARG_PROTO((int stop));
extern IDL_VPTR basic_type_conversion IDL_ARG_PROTO((int argc, IDL_VPTR argv[],
						     REGISTER int type));
extern void bitmap_landscape IDL_ARG_PROTO((IDL_RASTER_DEF *in,
					    IDL_RASTER_DEF *out, int y0));
extern IDL_VPTR byte IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void close_file IDL_ARG_PROTO((int argc, IDL_VPTR argv[], char *argk));
extern IDL_VPTR dbl IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void declare_exit_handler IDL_ARG_PROTO((IDL_EXIT_HANDLER_FUNC proc));
extern void del_str IDL_ARG_PROTO((IDL_STRING *str, LONG n));
extern void deltmp IDL_ARG_PROTO((REGISTER IDL_VPTR p));
extern void delvar IDL_ARG_PROTO((IDL_VPTR var));
extern void dither IDL_ARG_PROTO((UCHAR *data, int ncols, int nrows,
				  IDL_RASTER_DEF *r, int x0, int y0,
				  IDL_TV_STRUCT *secondary));
extern double double_scalar IDL_ARG_PROTO((REGISTER IDL_VPTR p));
extern void dup_str IDL_ARG_PROTO((REGISTER IDL_STRING *str, REGISTER LONG n));
extern int ensure_file_status IDL_ARG_PROTO((int action, int unit, int flags));
extern void ez_call IDL_ARG_PROTO((int argc, IDL_VPTR argv[],
				   IDL_EZ_ARG arg_struct[]));
extern void ez_call_cleanup IDL_ARG_PROTO((int argc, IDL_VPTR argv[],
					   IDL_EZ_ARG arg_struct[]));
extern char *filepath IDL_ARG_PROTO((char *pathbuf, char *file, char *ext,
		      int nsubdir, char **subdir));
extern char *filepath_from_root IDL_ARG_PROTO((char *pathbuf, IDL_STRING *root,
					       char *file, char *ext,
					       int nsubdir, char **subdir));
extern void freetmp IDL_ARG_PROTO((REGISTER IDL_VPTR p));
extern IDL_VPTR fix IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern IDL_VPTR flt IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void flush_file_unit IDL_ARG_PROTO((int unit));
extern void free_lun IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void get_file_stat IDL_ARG_PROTO((int unit, IDL_FILE_STAT *stat_blk));
extern void get_lun IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern int get_kbrd IDL_ARG_PROTO((int should_wait));
extern int get_kw_params IDL_ARG_PROTO((int argc, IDL_VPTR *argv, char *argk,
					IDL_KW_PAR *kw_list,
					IDL_VPTR *plain_args, int imask));
extern char *getscratch IDL_ARG_PROTO((IDL_VPTR *p, LONG n_elts,
				       LONG elt_size));
extern IDL_VPTR gettmp IDL_ARG_PROTO((void));
extern void get_user_info IDL_ARG_PROTO((IDL_USER_INFO *user_info));
extern IDL_VPTR get_var_addr IDL_ARG_PROTO((char *name));
extern IDL_VPTR get_var_addr1 IDL_ARG_PROTO((char *name, int ienter));
extern double graph_text IDL_ARG_PROTO((IDL_GR_PT *p, IDL_ATTR_STRUCT *ga,
			 IDL_TEXT_STRUCT *a,char *text));
extern char *idl_rline IDL_ARG_PROTO((char *s, int n, int unit, FILE *stream,
		       char *prompt, int opt));
extern IDL_VPTR import_array IDL_ARG_PROTO((int n_dim, LONG dim[], int type,
					    UCHAR *data));
extern void IDL_KWCleanup IDL_ARG_PROTO((int fcn));
extern IDL_VPTR lng IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void logit IDL_ARG_PROTO((char *s));
extern LONG long_scalar IDL_ARG_PROTO((REGISTER IDL_VPTR p));
extern ARRAY *make_array IDL_ARG_PROTO((int n_dim, LONG dim[],
					REGISTER IDL_VPTR var, LONG elt_size));
extern char *make_temp_array IDL_ARG_PROTO((int type, int n_dim, LONG dim[],
					    int init, IDL_VPTR *var));
extern void open_file IDL_ARG_PROTO((int argc, IDL_VPTR argv[], char *argk,
		      int access_mode, int extra_flags));
extern void polyfill_sfw IDL_ARG_PROTO((int *x, int *y, int n,
					IDL_POLYFILL_ATTR *s));
extern void pout_raw IDL_ARG_PROTO((int unit, char *buf, int n));
extern void print IDL_ARG_PROTO((int argc, IDL_VPTR *argv, char *argk));
extern void print_f IDL_ARG_PROTO((int argc, IDL_VPTR *argv, char *argk));
extern void raster_draw IDL_ARG_PROTO((IDL_GR_PT *p0, IDL_GR_PT *p1,
				       IDL_ATTR_STRUCT *a, IDL_RASTER_DEF *r));
extern void IDL_RasterDrawThick IDL_ARG_PROTO((IDL_GR_PT *p0, IDL_GR_PT *p1,
					       IDL_ATTR_STRUCT *a,
				  IDL_PRO_PTR routine, int dot_width));
extern void raster_polyfill IDL_ARG_PROTO((int *x, int *y, int n,
					   IDL_POLYFILL_ATTR *p,
					   IDL_RASTER_DEF *r));
extern IDL_VPTR ret_str_as_STRING IDL_ARG_PROTO((char *s));
extern void rgb_to_hls IDL_ARG_PROTO((UCHAR *r, UCHAR *g, UCHAR *b, float *h,
				      float *l, float *s, int n));
extern void rgb_to_hsv IDL_ARG_PROTO((UCHAR *r, UCHAR *g, UCHAR *b, float *h,
				      float *s, float *v, int n));
extern void set_file_close IDL_ARG_PROTO((int unit, int allow));
extern void store_scalar IDL_ARG_PROTO((IDL_VPTR dest, int type,
					IDL_ALLTYPES *value));
extern void str_ensure_length IDL_ARG_PROTO((IDL_STRING *s, int n));
extern void str_store IDL_ARG_PROTO((IDL_STRING *s, char *fs));
extern IDL_VPTR string IDL_ARG_PROTO((int argc, IDL_VPTR argv[], char *argk));
#ifdef OS_HAS_TTYS
extern void terminal_raw IDL_ARG_PROTO((int to_from, int fnin));
extern void tt_raw_write IDL_ARG_PROTO((char *buff, int nchars));
#endif
extern int unit_eof IDL_ARG_PROTO((int unit));
extern IDL_VPTR ur_bytscl IDL_ARG_PROTO((int argc, IDL_VPTR *argv,char *argk));
extern int ur_free IDL_ARG_PROTO((char *p, int action));
extern IDL_VPTR ur_complex IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern int ur_main IDL_ARG_PROTO((int action, int argc, char *argv[]));
extern char *ur_malloc IDL_ARG_PROTO((unsigned int size, int action));
extern void ur_wait IDL_ARG_PROTO((int argc, IDL_VPTR argv[]));
extern void var_copy IDL_ARG_PROTO((REGISTER IDL_VPTR src,
				    REGISTER IDL_VPTR dst));
extern void var_error IDL_ARG_PROTO((int code, IDL_VPTR var, int action));
extern char *varname IDL_ARG_PROTO((IDL_VPTR v));
extern void widget_stub_lock IDL_ARG_PROTO((int set));
extern char *widget_stub_lookup IDL_ARG_PROTO((unsigned long id));
extern void widget_issue_stub_event IDL_ARG_PROTO((char *rec, LONG value));
extern void widget_set_stub_ids IDL_ARG_PROTO((char *rec, unsigned long t_id,
					       unsigned long b_id));
extern void widget_get_stub_ids IDL_ARG_PROTO((char *rec, unsigned long *t_id,
					       unsigned long *b_id));
extern void widget_x_send_bogus_cmsg IDL_ARG_PROTO((void *rec));

#ifdef VMS
extern int for_getstring(IDL_VPTR param, struct dsc$descriptor *dest, int *n);
extern void for_store_scalar(IDL_VPTR dest, int *type, IDL_ALLTYPES *scalar);
extern IDL_VPTR for_GETTMP(int *type);
extern int for_getdims(IDL_VPTR var, int *ndims, int dimblk[], char **addr);
extern IDL_VPTR convert_type(IDL_VPTR v, UCHAR *typ);
extern void crearr(IDL_VPTR var, int *ndims, int *dimblk);
extern void tstdeltmp(IDL_VPTR var);
extern void copyvar(IDL_VPTR src, IDL_VPTR dest);
extern void printerrmsg(int *istatus);
extern void idlerr(struct dsc$descriptor *text);
extern void io_error(struct dsc$descriptor *text, int status);
extern void for_chkpar_error(char *msg);
extern int get_kbint(void);
extern long call_v1(int argc, IDL_VPTR *argv, int (* routine)());
extern IDL_VPTR FLOAT_IDL(IDL_VPTR v);
extern IDL_VPTR LONG_IDL(IDL_VPTR v);
extern IDL_VPTR DOUBLE(IDL_VPTR v);
extern IDL_VPTR BYTE_V1(IDL_VPTR v);
extern IDL_VPTR FIX_V1(IDL_VPTR v);
extern IDL_VPTR COMPLEX_V1(IDL_VPTR v);
extern IDL_VPTR STRING_v1(IDL_VPTR v);
extern long GETLONG_SCL(IDL_VPTR v);
extern float GETFLOAT_SCL(IDL_VPTR v);
extern double GETDOUBLE_SCL(IDL_VPTR v);
extern long BYTE1(UCHAR *c);
extern long INTEGER2(short *s);
extern long INTEGER4(long *l);
extern void STORE1(UCHAR *dst, UCHAR *src);
extern void STORE2(short *dst, short *src);
extern void STORE4(long *dst, long *src);
extern int FOR_CHKPAR();
#endif

#endif				/* obsolete_IDL_DEF */

