type VAL = () => Any
type BOOL = VAL => VAL => VAL

def one: VAL = () => 1
def two: VAL = () => 2

def IF = (cond: BOOL) => (than: VAL) => (els: VAL) => cond(than)(els)

def TRUE: BOOL = (than: VAL) => (els: VAL) => than
def FALSE: BOOL = (than: VAL) => (els: VAL) => els

IF(FALSE)(one)(two)()
IF(TRUE)(one)(two)()

//IF(IF(TRUE)(TRUE)(TRUE))(one)(two)