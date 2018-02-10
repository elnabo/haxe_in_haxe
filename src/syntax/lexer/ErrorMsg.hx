package syntax.lexer;

enum ErrorMsg {
	Invalid_character(i:Int);
	Unterminated_string;
	Unterminated_regexp;
	Unclosed_comment;
	Unclosed_code;
	Invalid_escape(char:Int);
	Invalid_option;
}