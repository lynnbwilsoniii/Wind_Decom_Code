/* windlib.h - C language definitions for windlib functions
*/

#define W_OK 1
#define W_EOF 82

extern int w_channel_open();          
extern int w_channel_select();
extern int w_channel_position();
extern int w_channel_filename();
extern int w_channel_close();

extern int w_messages_off();
extern int w_messages_on();
extern int w_version();

extern int w_item_i4();                        /* C routine */
extern int w_item_r4();                        /* C routine */
extern int w_item_r8();                        /* C routine */
extern int w_item_char();                      /* C routine */

extern int w_item_xlate();
extern int w_event();
extern int w_status();

extern int w_ur8_from_ydoy();
extern int w_ur8_from_ymd();
extern int w_ur8_to_string();
extern int w_ur8_to_string_fr();
extern int w_ur8_to_ydoy();
extern int w_ur8_to_ymd();
extern int w_ur8_to_ymd_i();
extern int w_ur8_from_ymd_i();
extern int w_ur8_to_epoch();
extern int w_ur8_from_epoch();

