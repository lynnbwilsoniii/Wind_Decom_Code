/* routine_def.h - wind.waves TM library routine definitions.  Note that
   most routines are in FORTRAN unless otherwise indicated.
*/

extern int w_channel_open_();		/* newer routines, post Nov-94 */
extern int w_channel_select();
extern int w_channel_position_();
extern int w_channel_filename_();
extern int w_channel_close_();

extern int w_messages_off_();
extern int w_messages_on_();
extern int w_version_();

extern int w_item_i4_();			/* C routine */
extern int w_item_r4_();			/* C routine */
extern int w_item_r8_();			/* C routine */
extern int w_item_char_();			/* C routine */

extern int w_item_xlate_();
extern int w_event_();
extern int w_status_();

extern int w_ur8_from_ydoy_();
extern int w_ur8_from_ymd_();
extern int w_ur8_to_string_();
extern int w_ur8_to_string_fr_();
extern int w_ur8_to_ydoy_();
extern int w_ur8_to_ymd_();
extern int w_ur8_to_ymd_i_();
extern int w_ur8_from_ymd_i_();
extern int w_ur8_to_epoch_();
extern int w_ur8_from_epoch_();

extern int wind_tm_open_channel_();        /* FORTRAN routines, pre Nov-94 */
extern int wind_tm_close_channel_();
extern int wind_tm_get_filename_();
extern int wind_tm_set_messages_off_();
extern int wind_tm_set_messages_on_();
extern int wind_tm_version_();
extern int wind_tm_eof_();

extern int wind_tm_get_event_();
extern int wind_tm_get_next_event_();
extern int wind_tm_get_previous_event_();
extern int wind_tm_get_item_();
extern int wind_tm_xlate_item_();

extern int wind_tm_set_wait_();
extern int wind_tm_set_nowait_();

extern int wind_tm_get_mfmf_();
extern int wind_tm_get_stream_mfmf_();
extern int wind_tm_get_next_mfmf_();
extern int wind_tm_get_earliest_mfmf_();
extern int wind_tm_get_latest_mfmf_();

extern int wind_tm_decrement_packet_();
extern int wind_tm_increment_packet_();
extern int wind_tm_decrement_mfmf_();
extern int wind_tm_increment_mfmf_();
extern int wind_tm_delta_mfmf_();

extern int wind_tm_bit_rate_();
extern int wind_tm_get_word_();
extern int wind_tm_get_minor_frame_();
extern int wind_tm_get_major_frame_();
extern int wind_tm_get_packet_();
extern int wind_tm_get_hk_();
extern int wind_tm_get_test_();
extern int wind_tm_get_step_();

extern int wind_tm_scet_to_mfmf_();
extern int wind_tm_mfmf_to_scet_();
extern int wind_tm_ert_to_mfmf_();
extern int wind_tm_mfmf_to_ert_();
