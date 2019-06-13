%%
%%  WS2801 led strip controller
%%
-module(ws2801_gpio).

-compile(export_all).

-define(CLCK_PIN,  23).   %% BLUE
-define(DATA_PIN,  24).   %% GREEN

init() ->
    application:start(gpio),
    gpio:init_direct(?DATA_PIN),
    gpio:set_direction(?DATA_PIN, low),
    gpio:init_direct(?CLCK_PIN),
    gpio:set_direction(?CLCK_PIN, low),
    ok.

%% create a strip of 150 
new() ->
    N = 5*30,
    new(N, {0,0,0}).

new(N, Color) ->
    list_to_tuple(lists:duplicate(N, Color)).

demo() ->
    Stripe = new(),
    N = tuple_size(Stripe),
    loop(0, N, Stripe).

loop(I, N, Stripe0) ->
    write(Stripe0),
    timer:sleep(500),
    Stripe1 = set(I, {0,0,0}, Stripe0),
    I1 = (I+1) rem N,
    Stripe2 = set(I1, {255,0,0}, Stripe1),
    loop(I1, N, Stripe2).

get(I, Stripe) ->
    element(I+1, Stripe).

set(I, Color, Stripe) ->
    setelement(I+1, Stripe, Color).
%% wait at least 0.5 ms = 500 us  between refresh 
write(Stripe) ->
    write_(1, Stripe).

write_(I, Stripe) when I >= 1, I =< tuple_size(Stripe) ->
    {R,G,B} = element(I, Stripe),
    write_rgb(R,G,B),
    write_(I+1, Stripe);
write_(_I, _Stripe) ->
    ok.

write_rgb(R,G,B) ->
    write_byte(R),
    write_byte(G),
    write_byte(B).

write_byte(B) ->
    [ pulse(I) || <<I:1>> <= <<B:8>> ].

pulse(0) ->
    gpio:clr(?DATA_PIN),
    gpio:set(?CLCK_PIN),
    gpio:clr(?CLCK_PIN);
pulse(1) ->
    gpio:set(?DATA_PIN),
    gpio:set(?CLCK_PIN),
    gpio:clr(?CLCK_PIN).
