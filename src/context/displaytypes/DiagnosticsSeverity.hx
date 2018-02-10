package context.displaytypes;


class DiagnosticsSeverity {

	public static function toInt (t:context.displaytypes.diagnosticsseverity.T) : Int {
		return switch(t) {
			case Error : 1;
			case Warning : 2;
			case Information : 3;
			case Hint : 4;
	}
}