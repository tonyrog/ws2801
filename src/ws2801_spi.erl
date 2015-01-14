%%
%%  WS2801 spi led strip controller
%%
-module(ws2801_spi).

-compile(export_all).

-define(SPEED, 1000000).   %% 1MHz
-define(BUS, 0).
-define(CHIP, 1).

start() ->
    start(?BUS,?CHIP).

stop() ->
    stop(?BUS, ?CHIP).

start(Bus,Chip) ->
    init(Bus,Chip),
    demo(Bus,Chip).

stop(Bus,Chip) ->
    spi:close(Bus, Chip).

init(Bus,Chip) ->
    application:start(spi),
    spi:open(Bus,Chip),
    spi:set_mode(Bus,Chip,0).

set_color(Color) ->
    set_color(?BUS,?CHIP,Color,?SPEED).

set_color(Bus,Chip,Color,Speed) ->
    Pixmap = new(5*32, Color),
    write(Bus,Chip,Pixmap,Speed).
    

%% create a stip of
new() ->
    new(5*32).

new(N) ->
    new(N, {0,0,0}).

new(N, Color) ->
    Pixmap = epx:pixmap_create(N, 1, rgb),
    epx:pixmap_fill(Pixmap, Color),
    Pixmap.

demo(Bus,Chip) ->
    demo(Bus,Chip,5*32).

demo(Bus,Chip,Width) ->
    Server = server(Bus,Chip,Width),
    sprite(sp1, Server, {127,0,0}, 0, 5, Width, 1, 10, bounce),
    sprite(sp2, Server, {127,127,127}, Width, 10, Width, -1, 5, wrap),
    sprite(sp3, Server, {0,127,0}, Width div 2, 1, Width, -1, 20, wrap),
    Server.
    

sprite(Id, Server, Color, X, Xl, W, Dir, Speed, Option) ->
    spawn_link(
      fun() ->
	      sprite_loop(Id, Server, Color, X, Xl, W, Dir, Speed, Option)
      end).

sprite_loop(Id, Server, Color, X, Xl, W, Xd, Speed, Option) ->
    Server ! {set, Id, X, Xd, Xl, Color},
    X1 = X + Xd,
    {X2,Xd1} = if X1 < 0 -> 
			case Option of
			    bounce -> {0, -Xd};
			    wrap -> {W-1, Xd}
			end;
		   X1 >= W -> 
			case Option of
			    bounce -> {W-1,-Xd};
			    wrap -> {0, Xd}
			end;
		   true ->
			{X1, Xd}
		end,
    receive
    after Speed ->
	    sprite_loop(Id, Server, Color, X2, Xl, W, Xd1, Speed, Option)
    end.
	       

server(Bus,Chip,N) ->
    spawn_link(fun() -> server_loop(Bus,Chip,new(N), 0, 0) end).

server_loop(Bus,Chip,Pixmap, 0, Max) when Max > 0 ->
    update(Bus,Chip,Pixmap),
    server_loop(Bus,Chip,Pixmap, Max, Max);
server_loop(Bus,Chip,Pixmap, I, Max) ->
    receive
	{set, Id, X, Xd, Xl, Color} ->
	    {Max1,Is} = case get(Id) of 
			    undefined -> {Max+1,0};
			    _ -> {Max,1}
			end,
	    put(Id, {X, Xd, Xl, Color}),
	    server_loop(Bus,Chip,Pixmap, I-Is, Max1)
    after 10 ->
	    update(Bus,Chip,Pixmap),
	    server_loop(Bus,Chip,Pixmap, Max, Max)
    end.
		
update(Bus,Chip,Pixmap) ->
    W = epx:pixmap_info(Pixmap, width),
    epx:pixmap_fill(Pixmap, {0,0,0}),
    lists:foreach(
      fun({_,{X,Xd,Xl,Color}}) ->
	      plot(Pixmap,X, Xd, Xl, W, Color)
      end, get()),
    write(Bus,Chip,Pixmap).    

plot(_Pixmap, _X, _Xd, 0, _W, _Color) ->
    ok;
plot(Pixmap, X, Xd, Xl, W, Color) ->
    epx:pixmap_put_pixel(Pixmap, X, 0, Color),
    if Xd < 0 ->
	    X1 = wrap(X-1,W),
	    plot(Pixmap,X1,Xd,Xl-1,W,Color);
       Xd >= 0 ->
	    X1 = wrap(X+1,W),
	    plot(Pixmap,X1,Xd,Xl-1,W,Color)
    end.

wrap(X, W) when X >= W -> 0;
wrap(X, W) when X =< 0 -> W - 1;
wrap(X,_W) -> X.

%% wait at least 0.5 ms = 500 us  between refresh 
write(Bus,Chip,Pixmap) ->
    write(Bus,Chip,Pixmap,?SPEED).
write(Bus,Chip,Pixmap,Speed) ->
    N = epx:pixmap_info(Pixmap, width),
    Data = epx:pixmap_get_pixels(Pixmap, 0, 0, N, 1),
    spi:transfer(Bus,Chip,Data,0,0,Speed,8,0).
