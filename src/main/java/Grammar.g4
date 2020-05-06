grammar Grammar;

cfg_rules : cfg_rules cfg_rule | cfg_rule;

cfg_rule : space* start_nonterminal space*  '->' space* rhs space* end;

rhs : str | rhs space* delimeter space* str;
str: (nonterminal | terminal)+;
start_nonterminal: nonterminal;
nonterminal : NAME;
terminal : SYMBOL;
delimeter : DEL;
space: WHITESPACE;

end: newline | eof;
newline: NEWLINE;
eof: EOF;

NAME: '<'[A-Z]+'>';
DEL: '|';
SYMBOL: ('/'[a-z]+ | '<eps>');
WS: [\t]+ -> skip;
WHITESPACE: ' ' -> skip;
NEWLINE: [\n]+;
ANY: .;
