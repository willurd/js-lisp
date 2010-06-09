/**
 * Returns a lispy string representation of the given value.
 */
function toLisp (object) {
	if (object === null) {
		return "nil";
	}
	if (object === true) {
		return "t";
	}
	if (object === false) {
		return "f";
	}
	if (object instanceof Symbol) {
		return String(object);
	}
	if (object instanceof Keyword) {
		return ":" + String(object);
	}
	if (object instanceof Array) {
		return "(" + object.map(toLisp).join(" ") + ")";
	}
	return toJSON(object, true);
}
