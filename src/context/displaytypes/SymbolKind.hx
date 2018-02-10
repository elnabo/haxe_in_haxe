package context.displaytypes;

class SymbolKind {

	public static function toInt (sk:context.displaytypes.symbolkind.T) {
		return switch(sk) {
			case Class : 1;
			case Interface : 2;
			case Enum : 3;
			case Typedef : 4;
			case Abstract : 5;
			case Field : 6;
			case Property : 7;
			case Method : 8;
			case Constructor : 9;
			case Function : 10;
			case Variable : 11;
		};
	}
}