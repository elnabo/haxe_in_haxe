package context.displaytypes;

enum DiagnosticsSeverity_T {
	Error;
	Warning;
	Information;
	Hint;
}

abstract DiagnosticsSeverity(DiagnosticsSeverity_T) from DiagnosticsSeverity_T to DiagnosticsSeverity_T {
	public static function toInt (t:context.displaytypes.DiagnosticsSeverity) : Int {
		return switch(t) {
			case Error : 1;
			case Warning : 2;
			case Information : 3;
			case Hint : 4;
		}
	}
}