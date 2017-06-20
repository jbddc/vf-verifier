pre x >= 0 && y >= 0

r = x ;
q = 0 ;

try {
    if (y == 0) then { throw ; }

    r = x ;
    q = 0 ;

    while (y <= r) {
        r = r - y ;
        q = q + 1 ;
    }
} catch {
    q = 1000 ;
    r = 0;
}

postn (r >= 0 && r < y && q * y + r == x) || (y == 0 && q == 1000 && r == 0)
poste false
