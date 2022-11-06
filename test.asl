type Arch of enumeration { Arch64, Arch32 }

constant arch = Arch64;
constant size = if arch == Arch64 then 64 else 32 end;

func dontabort()
	pass
endfunc

func main()
	x = 3 + 4;
	if (size == 64) then
		y = 4 + - size;
		if (arch == Arch32) then
			abort()
		else
			dontabort()
		end
	else
		z = x * y
	end
endfunc
