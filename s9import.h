/*
 * S9core Toolkit, Mk IVd
 * By Nils M Holm, 2007-2019
 * In the public domain
 *
 * Under jurisdictions without a public domain, the CC0 applies:
 * https://creativecommons.org/publicdomain/zero/1.0/
 */

/*
 * Remove S9_ and s9_ prefixes from common definitions
 */

#define cell	s9_cell
#define counter	s9_counter

#define special_p	s9_special_p

#define NIL		S9_NIL
#define TRUE		S9_TRUE
#define FALSE		S9_FALSE
#define END_OF_FILE	S9_END_OF_FILE
#define UNDEFINED	S9_UNDEFINED
#define UNSPECIFIC	S9_UNSPECIFIC
#define VOID		S9_VOID

#define T_ANY		S9_T_ANY
#define T_BOOLEAN	S9_T_BOOLEAN
#define T_CHAR		S9_T_CHAR
#define T_INPUT_PORT	S9_T_INPUT_PORT
#define T_INTEGER	S9_T_INTEGER
#define T_LIST		S9_T_LIST
#define T_OUTPUT_PORT	S9_T_OUTPUT_PORT
#define T_PAIR		S9_T_PAIR
#define T_PRIMITIVE	S9_T_PRIMITIVE
#define T_FUNCTION	S9_T_FUNCTION
#define T_REAL		S9_T_REAL
#define T_STRING	S9_T_STRING
#define T_SYMBOL	S9_T_SYMBOL
#define T_SYNTAX	S9_T_SYNTAX
#define T_VECTOR	S9_T_VECTOR
#define T_CONTINUATION	S9_T_CONTINUATION
#define T_FIXNUM	S9_T_FIXNUM
#define T_NONE		S9_T_NONE

#define USER_SPECIALS	S9_USER_SPECIALS

#define nl	s9_nl

#define string		s9_string
#define string_len	s9_string_len
#define symbol_name	s9_symbol_name
#define symbol_len	s9_symbol_len
#define vector		s9_vector
#define vector_link	s9_vector_link
#define vector_index	s9_vector_index
#define vector_size	s9_vector_size
#define vector_len	s9_vector_len
#define port_no		s9_port_no
#define fixval		s9_fixval
#define small_int_value	s9_small_int_value
#define char_value	s9_char_value
#define prim_slot	s9_prim_slot
#define prim_info	s9_prim_info

#define tag	s9_tag

#define car	s9_car
#define cdr	s9_cdr
#define caar	s9_caar
#define cadr	s9_cadr
#define cdar	s9_cdar
#define cddr	s9_cddr
#define caaar	s9_caaar
#define caadr	s9_caadr
#define cadar	s9_cadar
#define caddr	s9_caddr
#define cdaar	s9_cdaar
#define cdadr	s9_cdadr
#define cddar	s9_cddar
#define cdddr	s9_cdddr
#define caaaar	s9_caaaar
#define caaadr	s9_caaadr
#define caadar	s9_caadar
#define caaddr	s9_caaddr
#define cadaar	s9_cadaar
#define cadadr	s9_cadadr
#define caddar	s9_caddar
#define cadddr	s9_cadddr
#define cdaaar	s9_cdaaar
#define cdaadr	s9_cdaadr
#define cdadar	s9_cdadar
#define cdaddr	s9_cdaddr
#define cddaar	s9_cddaar
#define cddadr	s9_cddadr
#define cdddar	s9_cdddar
#define cddddr	s9_cddddr

#define Car		S9_car
#define Cdr		S9_cdr
#define Tag		S9_tag
#define Vectors		S9_vectors
#define Nullvec		S9_nullvec
#define Stack		S9_stack
#define Primitives	S9_primitives
#define Zero		S9_zero
#define One		S9_one
#define Two		S9_two
#define Ten		S9_ten
#define Epsilon		S9_epsilon
#define Ports		S9_ports
#define Input_port	S9_input_port
#define Output_port	S9_output_port
#define Error_port	S9_error_port

#define eof_p		s9_eof_p
#define undefined_p	s9_undefined_p
#define unspecific_p	s9_unspecific_p
#define boolean_p	s9_boolean_p
#define constant_p	s9_constant_p
#define integer_p	s9_integer_p
#define number_p	s9_number_p
#define primitive_p	s9_primitive_p
#define function_p	s9_function_p
#define continuation_p	s9_continuation_p
#define real_p		s9_real_p
#define fix_p		s9_fix_p
#define char_p		s9_char_p
#define syntax_p	s9_syntax_p
#define input_port_p	s9_input_port_p
#define output_port_p	s9_output_port_p
#define symbol_p	s9_symbol_p
#define vector_p	s9_vector_p
#define string_p	s9_string_p
#define atom_p		s9_atom_p
#define pair_p		s9_pair_p
#define small_int_p	s9_small_int_p
#define type_tag	s9_type_tag

#define cons		s9_cons
#define new_atom	s9_new_atom
#define save		s9_save

#define bignum_negative_p	s9_bignum_negative_p
#define bignum_zero_p		s9_bignum_zero_p
#define bignum_positive_p	s9_bignum_positive_p

#define Make_real		S9_make_real
#define Real_flags		S9_real_flags
#define Real_exponent		S9_real_exponent
#define Real_mantissa		S9_real_mantissa
#define REAL_NEGATIVE		S9_REAL_NEGATIVE
#define Real_negative_flag	S9_real_negative_flag
#define Real_zero_p		S9_real_zero_p
#define Real_negative_p		S9_real_negative_p
#define Real_positive_p		S9_real_positive_p
#define Real_negate		S9_real_negate

#define GC_stack	S9_gc_stack
#define GC_stkptr	S9_gc_stkptr

#ifndef S9_S9CORE
 #define apply_prim		s9_apply_prim
 #define argv_to_list		s9_argv_to_list
 #define asctol			s9_asctol
 #define bignum_abs		s9_bignum_abs
 #define bignum_add		s9_bignum_add
 #define bignum_divide		s9_bignum_divide
 #define bignum_equal_p		s9_bignum_equal_p
 #define bignum_even_p		s9_bignum_even_p
 #define bignum_less_p		s9_bignum_less_p
 #define bignum_multiply	s9_bignum_multiply
 #define bignum_negate		s9_bignum_negate
 #define bignum_shift_left	s9_bignum_shift_left
 #define bignum_shift_right	s9_bignum_shift_right
 #define bignum_subtract	s9_bignum_subtract
 #define bignum_to_int		s9_bignum_to_int
 #define bignum_to_real		s9_bignum_to_real
 #define bignum_to_string	s9_bignum_to_string
 #define blockread		s9_blockread
 #define blockwrite		s9_blockwrite
 #define close_input_string	s9_close_input_string
 #define close_port		s9_close_port
 #define cons3			s9_cons3
 #define conses			s9_conses
 #define cons_stats		s9_cons_stats
 #define copy_string		s9_copy_string
 #define count			s9_count
 #define dump_image		s9_dump_image
 #define error_port		s9_error_port
 #define exponent_chars		s9_exponent_chars
 #define fatal			s9_fatal
 #define find_symbol		s9_find_symbol
 #define flat_copy		s9_flat_copy
 #define flush			s9_flush
 #define gc			s9_gc
 #define gc_verbosity		s9_gc_verbosity
 #define gcv			s9_gcv
 #define get_counters		s9_get_counters
 #define image_vars		s9_image_vars
 #define input_port		s9_input_port
 #define inport_open_p		s9_inport_open_p
 #define integer_string_p	s9_integer_string_p
 #define intern_symbol		s9_intern_symbol
 #define int_to_bignum		s9_int_to_bignum
 #define io_reset		s9_io_reset
 #define io_status		s9_io_status
 #define length			s9_length
 #define load_image		s9_load_image
 #define lock_port		s9_lock_port
 #define make_char		s9_make_char
 #define make_integer		s9_make_integer
 #define make_port		s9_make_port
 #define make_primitive		s9_make_primitive
 #define make_real		s9_make_real
 #define make_string		s9_make_string
 #define make_symbol		s9_make_symbol
 #define make_vector		s9_make_vector
 #define mem_error_handler	s9_mem_error_handler
 #define mkfix			s9_mkfix
 #define new_port		s9_new_port
 #define new_vec		s9_new_vec
 #define open_input_port	s9_open_input_port
 #define open_input_string	s9_open_input_string
 #define open_output_port	s9_open_output_port
 #define output_port		s9_output_port
 #define outport_open_p		s9_outport_open_p
 #define port_eof		s9_port_eof
 #define print_bignum		s9_print_bignum
 #define print_expanded_real	s9_print_expanded_real
 #define print_real		s9_print_real
 #define print_sci_real		s9_print_sci_real
 #define printer_limit		s9_printer_limit
 #define prints			s9_prints
 #define read_counter		s9_read_counter
 #define readc			s9_readc
 #define real_abs		s9_real_abs
 #define real_add		s9_real_add
 #define real_approx_p		s9_real_approx_p
 #define real_ceil		s9_real_ceil
 #define real_divide		s9_real_divide
 #define real_equal_p		s9_real_equal_p
 #define real_exponent		s9_real_exponent
 #define real_floor		s9_real_floor
 #define real_integer_p		s9_real_integer_p
 #define real_less_p		s9_real_less_p
 #define real_mantissa		s9_real_mantissa
 #define real_multiply		s9_real_multiply
 #define real_negate		s9_real_negate
 #define real_negative_p	s9_real_negative_p
 #define real_positive_p	s9_real_positive_p
 #define real_power		s9_real_power
 #define real_round		s9_real_round
 #define real_sqrt		s9_real_sqrt
 #define real_subtract		s9_real_subtract
 #define real_to_bignum		s9_real_to_bignum
 #define real_to_string		s9_real_to_string
 #define real_trunc		s9_real_trunc
 #define real_zero_p		s9_real_zero_p
 #define rejectc		s9_rejectc
 #define reset_counter		s9_reset_counter
 #define reset_std_ports	s9_reset_std_ports
 #define run_stats		s9_run_stats
 #define set_input_port		s9_set_input_port
 #define set_node_limit		s9_set_node_limit
 #define set_output_port	s9_set_output_port
 #define set_printer_limit	s9_set_printer_limit
 #define set_vector_limit	s9_set_vector_limit
 #define string_numeric_p	s9_string_numeric_p
 #define string_to_bignum	s9_string_to_bignum
 #define string_to_number	s9_string_to_number
 #define string_to_real		s9_string_to_real
 #define string_to_symbol	s9_string_to_symbol
 #define symbol_ref		s9_symbol_ref
 #define symbol_table		s9_symbol_table
 #define symbol_to_string	s9_symbol_to_string
 #define typecheck		s9_typecheck
 #define unlock_port		s9_unlock_port
 #define unsave			s9_unsave
 #define writec			s9_writec
#endif

