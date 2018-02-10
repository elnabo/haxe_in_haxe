package syntax.parser;

enum ErrorMsg {
	Unexpected (token:core.Ast.Token);
	Duplicate_default;
	Missing_semicolon;
	Unclosed_macro;
	Unimplemented;
	Missing_type;
	Custom(s:String);
}