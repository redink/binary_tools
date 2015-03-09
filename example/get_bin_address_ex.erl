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
