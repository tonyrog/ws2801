%%
%%  WS2801 spi led strip controller
%%
-module(ws2801_spi).

-compile(export_all).

-define(SPEED, 2000000).   %% 2MHz

start() ->
    application:start(spi),
    spi:open(0, 0),
    spi:set_mode(0,0,0).

stop() ->
    spi:close(0, 0).

%% create a stip of
new() ->
    new(5*32, {0,0,0}).

new(N, Color) ->
    Pixmap = epx:pixmap_create(N, 1, rgb),
    epx:pixmap_fill(Pixmap, Color),
    Pixmap.

demo() ->
    Pixmap = new(),
    N = epx:pixmap_info(Pixmap, width),
    loop(0, N, Pixmap).

loop(I, N, Pixmap) ->
    write(Pixmap),
    timer:sleep(7),
    set(I, {0,0,0}, Pixmap),
    I1 = (I+1) rem N,
    set(I1, {255,0,0}, Pixmap),
    loop(I1, N, Pixmap).

get(I, Pixmap) ->
    <<R,G,B>> = epx:pixmap_get_pixels(Pixmap, I, 0, 1, 1),
    {R,G,B}.

set(I, Color, Pixmap) ->
    epx:pixmap_set_pixel(Pixmap, I, 0, Color).
    
%% wait at least 0.5 ms = 500 us  between refresh 
write(Pixmap) ->
    N = epx:pixmap_info(Pixmap, width),
    Data = epx:pixmap_get_pixels(Pixmap, 0, 0, N, 1),
    spi:transfer(0,0,Data,0,0,?SPEED,8,0).

