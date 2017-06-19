pre x >= 0 && y >= 0

int x = 16 ;
int y = 4 ;

int r = 0 ;
int q = 0 ;

r = x ;
q = 0 ;

try {
    if (y == 0) then {throw;}

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

postn y == 0 && q == 1000 && r == 0
poste false
