fun readInt input =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

fun reverse(x,z) = if null(x) then z
        else reverse(tl(x),hd(x)::z);

fun rev(x) = reverse(x,[]);

fun printn 0 = print"\n"
    | printn n = print (Int.toString(n)); printn 0;

fun pri nil = ( print "]"; print "\n" )
   | pri (h::[]) = ( print (Int.toString (h)) ; pri [])
   | pri (h::t) = ( print (Int.toString (h)) ; print "," ; pri t );

fun prin l = (print "["; pri l);

fun fst (x,_) = x;
fun snd (_,x) = x;

fun halve nil = (nil, nil)
| halve [a] = ([a], nil)
| halve (a::b::cs)=
let 
val (x, y) = halve cs
in 
(a::x, b::y)
end;

fun parse file =
    let 
        val inStream = TextIO.openIn file
        val n = readInt inStream
        val n = 3*readInt inStream
        val _ = TextIO.inputLine inStream

        fun readInts 0 acc = acc
            | readInts i acc = readInts (i-1)(readInt inStream::acc)
    in
        rev ( readInts n [])
    end;

fun put_graph a l = 
    if ( l = []) then a
    else (let
            val i = hd(l)
            val list = tl (l)
            val j = hd(list)
            val list = tl (list)
            val w = hd (list)
            val t = tl (list)
            fun done i j w t = 
                if (i<j) then Array2.update(a, i-1, j-1, w)
                else Array2.update(a, j-1, i-1, w)
                

        in
        done i j w t; 
        put_graph a t
        end);

fun minkey key mstset n minimum  =
    (
    let val v =ref 0
        fun done minimum =
            if (Array.sub(mstset,!v)=0 andalso Array.sub(key, !v) < Array.sub(minimum,0))  then 
                (
                Array.update(minimum,0,(Array.sub(key, !v))); Array.update(minimum, 1, !v); minimum)
            else (
                minimum)

    in while !v < n do 
        (done minimum;
        v:= !v+1)

    end; minimum);


fun prims arr m n parent =
    (let 
        val key  = Array.array(n, valOf Int.maxInt) 
        val mstset = Array.array(n,0)
        val minimum = Array.array(2,valOf Int.maxInt)
        val i =  ref 0
        
    in while !i < n do 
        (Array.update(parent, 0, ~1);Array.update(key, 0, 0);
        minkey key mstset n minimum;          
        let val j = ref 0
            val u = Array.sub(minimum, 1)
            fun mst arr n !j key parent mstset u = 
                if (Array2.sub(arr, u, !j)>=0 andalso Array.sub(mstset, !j) = 0 andalso Array2.sub(arr, u, !j)<Array.sub(key,!j) ) then 
                    ((Array.update(parent, !j, u));
                        Array.update(key,!j, Array2.sub(arr, u, !j)))
                else if (Array2.sub(arr, !j, u)>=0 andalso Array.sub(mstset, !j) = 0 andalso Array2.sub(arr, !j, u)<Array.sub(key,!j) ) then 
                    ((Array.update(parent, !j, u));
                        Array.update(key,!j, Array2.sub(arr, !j, u)))
                else ()
        in while !j < n do 
            ( 
                Array.update(mstset, u, 1);
                mst arr n !j key parent mstset u; 
                j := !j+1
            )
        end;
        Array.update(minimum, 0, valOf Int.maxInt);Array.update(minimum, 1, valOf Int.maxInt);
        i := !i+1
        ) 
    end; parent);

fun max arr parent n answer=
    (let val k = ref 1
        fun check arr parent answer =
            if (Array.sub(answer, 0)<Array2.sub(arr, Array.sub(parent, !k), !k)) then 
                Array.update(answer, 0, Array2.sub(arr, Array.sub(parent, !k), !k))
            else if (Array.sub(answer, 0)<Array2.sub(arr, !k, Array.sub(parent, !k))) then 
                Array.update(answer, 0, Array2.sub(arr, !k, Array.sub(parent, !k)))
            else ()
    in while !k < n do
        (check arr parent answer;
        k := !k+1)
    end; answer);


fun min_fill file = 
    let 
        val list = parse file
        val inStream = TextIO.openIn file
        val n = readInt inStream
        val m = readInt inStream
        val _ = TextIO.inputLine inStream
        val arr = Array2.array (n, n, ~1)
        val parent = Array.array(n,0)
        val arr = put_graph arr list
        val parent = prims arr m n parent
        val answer = Array.array(1, 0)

    in 
        max arr parent n answer;
        print (Int.toString(Array.sub(answer, 0)));
        print ("\n")
    end;  