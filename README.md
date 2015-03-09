# binary_tools
`binary_tools` is a set of ancillary services function for Erlang binary data type. 

## dir tree
	$ tree 
	.
	├── README.md
	├── c_src
	│   └── binary_tools.c
	├── ebin
	├── example
	│   └── get_bin_address_ex.erl
	├── priv
	├── rebar
	├── rebar.config
	└── src
	    ├── binary_tools.app.src
	    └── binary_tools.erl

	5 directories, 7 files
`get_bin_address_ex` module is using example.

## get_bin_address
`get_bin_address` function could get one binary data's size and address in Erlang VM.

### example

```
$ cat example/get_bin_address_ex.erl 
-module(get_bin_address_ex).

-compile(export_all).

example() ->
    
    %% origin binary
    Origin = binary:copy(<<1,2,3>>),
    io:format(" ** origin binary is ~p,~n    address info ~p~n~n",
              [Origin, binary_tools:get_bin_address(Origin)]),
    
    %% first append binary
    FirstAppend = <<Origin/binary, <<4,5>>/binary>>,
    io:format(" ** after first append binary is ~p,~n    address info ~p~n",
             [FirstAppend, binary_tools:get_bin_address(FirstAppend)]),
    %% second append binary
    SecondAppend = <<FirstAppend/binary, <<1,2,3>>/binary>>,
    io:format(" ** after second append binary is ~p,~n    address info ~p~n~n",
             [SecondAppend, binary_tools:get_bin_address(SecondAppend)]),

    %% split binary
    [Split1, Split2] = binary:split(SecondAppend, <<4,5>>),
    io:format(" ** split binary split1 is ~p,~n    address info ~p~n",
             [Split1, binary_tools:get_bin_address(Split1)]),
    io:format(" ** split binary split2 is ~p,~n    address info ~p~n~n",
             [Split2, binary_tools:get_bin_address(Split2)]),

    %% binary part
    Part1 = binary:part(SecondAppend, 0, 3),
    io:format(" ** binary part start 0, part1 binary ~p,~n    address info ~p~n",
             [Part1, binary_tools:get_bin_address(Part1)]),
    Part2 = binary:part(SecondAppend, 1, 3),
    io:format(" ** binary part start 1 part2 binary,  ~p,~n    address info ~p~n",
             [Part2, binary_tools:get_bin_address(Part2)]).

```

And, the execute result is :

```
$ erl -pa ./ebin -pa ./example/
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.3  (abort with ^G)
1> get_bin_address_ex:example().
 ** origin binary is <<1,2,3>>,
    address info "bin: size=3, ptr=0x198c0308"

 ** after first append binary is <<1,2,3,4,5>>,
    address info "bin: size=5, ptr=0x19b00f40"
 ** after second append binary is <<1,2,3,4,5,1,2,3>>,
    address info "bin: size=8, ptr=0x19b011a0"

 ** split binary split1 is <<1,2,3>>,
    address info "bin: size=3, ptr=0x19b011a0"
 ** split binary split2 is <<1,2,3>>,
    address info "bin: size=3, ptr=0x19b011a5"

 ** binary part start 0, part1 binary <<1,2,3>>,
    address info "bin: size=3, ptr=0x19b011a0"
 ** binary part start 1 part2 binary,  <<2,3,4>>,
    address info "bin: size=3, ptr=0x19b011a1"
ok
```

Yes, you can see:

1, the binary address of `SecondAppend`, `Split1`, `Part1` same.

2, the binary address of `Split1`, `Split2` is consecutive; `Part1` and `Part2` too.
	

