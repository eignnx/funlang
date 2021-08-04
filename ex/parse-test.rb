{#
    This is a multiline comment!
    It has multiple lines!
    Can it nest? {#
        YES!
    #}
#}

def main[] do
    x = 10;
    y = 1;
    while x != 2 * y do
        intr.dbg_int[x];
        if x > y xor x < 2 then
            y = 2 - x;
            nop; # Careful: load bearing nop!
            intr.puts["hello, world!"];
        else
            x = 3 * y + 1;
        end
        x = x - 1;
    end
end