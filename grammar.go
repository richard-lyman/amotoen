package amotoen

var zws = Z(ES(" \t"))
var Grammar = O(S(Z(ES(" \t\n")), rule, zws, R('\n')))
var rule = S(key, zws, R(':'), R('='), zws, ruleBody)
var key = S(lower, Z(E(lower, upper, number)))
var lower = ES("abcdefghijklmnopqrstuvwxyz")
var upper = ES("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
var number = ES("0123456789")
var ruleBody = &DeferredState{}
var ruleBodyList = S(ruleBody, Z(S(zws, R(','), zws, ruleBody)))
var escapedUnicode = S(R('\\'), E(shortUnicode, longUnicode))
var shortUnicode = S(R('u'), hexRune, hexRune)
var longUnicode = S(R('U'), hexRune, hexRune, hexRune, hexRune)
var escapedRune = S(R('\\'), N(R('\x00')))
var seq = S(R('['), ruleBodyList, R(']'))
var either = S(R('{'), ruleBodyList, R('}'))
var eitherString = S(R('>'), R('`'), O(N(R('`'))), R('`'))
var zeroOrMore = S(R('*'), ruleBody)
var oneOrMore = S(R('+'), ruleBody)
var anyNot = S(R('!'), ruleBody)
var stringValue = S(R('`'), O(N(R('`'))), R('`'))
var hexRune = ES("0123456789abcdefABCDEF")

func init() {
	ruleBody.Set(E(
		escapedUnicode, escapedRune,
		seq, either, eitherString, zeroOrMore, oneOrMore, anyNot,
		stringValue, key,
	))
}
