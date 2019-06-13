%%
%%  WS2801 spi led strip controller
%%
-module(ws2801_spi).

-compile(export_all).

-define(SPEED, 1000000).   %% 1MHz
-define(BUS, 0).
-define(CHIP, 1).

-record(sprite, 
	{
	  x = 0    :: integer(),
	  dir = 0  :: integer(),
	  speed = 0 :: integer(),
	  length = 1 :: integer(),
	  scale = 1 :: integer(),
	  color = {0,0,0},    %% {R,G,B}
	  option = bounce
	}).


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
    Pixmap = new(10*32, Color),
    write(Bus,Chip,Pixmap,Speed).
    

%% create a stip of
new() ->
    new(10*32).

new(N) ->
    new(N, {0,0,0}).

new(N, Color) ->
    Pixmap = epx:pixmap_create(N, 1, rgb),
    epx:pixmap_fill(Pixmap, Color),
    Pixmap.

demo(Bus,Chip) ->
    demo(Bus,Chip,10*32,30).

demo(Bus,Chip,Width,N) ->
    Server = server(Bus,Chip,Width),
    lists:foreach(
      fun(Name) ->
	sprite(Name, Server, Width,
	       #sprite { x=rand:uniform(Width)-1,
			 color=random_color(),
			 length=random(5, 10),
			 dir=random(-1,1),
			 speed=random(90,100),
			 scale=random(0.5, 1.0),
			 option=bounce })
      end, lists:seq(1, N)),
    Server.

demo_blink(Bus,Chip,Width,N) ->
    Server = server(Bus,Chip,Width),
    lists:foreach(
      fun(Name) ->
	sprite(Name, Server, Width,
	       #sprite { x=rand:uniform(Width)-1,
			 color=random_color(),
			 length=3,   %% random(5, 10),
			 dir=0,      %% random(-1,1)
			 speed=100,  %% 10 times a second
			 scale=1,     %% colore scale
			 option=bounce })
      end, lists:seq(1, N)),
    Server.
    
sprite(Id, Server, Width, Sprite) ->
    spawn_link(
      fun() ->
	      sprite_loop(Id, Server, Width, Sprite)
      end).

sprite_loop(Id, Server, Width, Sprite) ->
    Server ! {set, Id, Sprite},
    {X2,Xd1} = 
	case Sprite#sprite.dir of
	    0 ->
		{rand:uniform(Width)-1, 0};
	    Dir ->
		X1 = Sprite#sprite.x + Dir,
		if X1 < 0 ->
			case Sprite#sprite.option of
			    bounce -> {0, -Dir};
			    wrap -> {Width-1, Dir}
			end;
		   X1 >= Width -> 
			case Sprite#sprite.option of
			    bounce -> {Width-1,-Dir};
			    wrap -> {0, Dir}
			end;
		   true ->
			{X1, Dir}
		end
	end,
    receive
    after Sprite#sprite.speed ->
	    sprite_loop(Id, Server, Width, Sprite#sprite { x=X2, dir=Xd1 })
    end.

server(Bus,Chip,N) ->
    spawn_link(fun() -> server_loop(Bus,Chip,new(N), 0, 0) end).

server_loop(Bus,Chip,Pixmap, 0, Max) when Max > 0 ->
    update(Bus,Chip,Pixmap),
    server_loop(Bus,Chip,Pixmap, Max, Max);
server_loop(Bus,Chip,Pixmap, I, Max) ->
    receive
	{set, Id, Sprite} ->
	    {Max1,Is} = case get(Id) of 
			    undefined -> {Max+1,0};
			    _ -> {Max,1}
			end,
	    put(Id, Sprite),
	    server_loop(Bus,Chip,Pixmap,I-Is,Max1)
    after 10 ->
	    update(Bus,Chip,Pixmap),
	    server_loop(Bus,Chip,Pixmap, Max, Max)
    end.
		
update(Bus,Chip,Pixmap) ->
    Width = epx:pixmap_info(Pixmap, width),
    epx:pixmap_fill(Pixmap, {0,0,0}),
    lists:foreach(
      fun({_, Sprite}) ->
	      draw(Pixmap, Width, Sprite)
      end, get()),
    write(Bus,Chip,Pixmap).

draw(Pixmap,Width,Sprite) ->
    case Sprite#sprite.dir of
	-1 ->
	    X0 = Sprite#sprite.x,
	    X1 = X0+Sprite#sprite.length-1,
	    plot(Pixmap,1, X0, X1, Width, Sprite);
	1 ->
	    X1 = Sprite#sprite.x,
	    X0 = X1+Sprite#sprite.length-1,
	    plot(Pixmap,1, X0,X1, Width, Sprite);
	0 ->
	    X0 = Sprite#sprite.x+(Sprite#sprite.length div 2),
	    X1 = X0+1,
	    plot(Pixmap,1,X0,X1, Width, Sprite)
    end.

plot(Pixmap, I, X0, X1, Width,
     Sprite = #sprite { dir=Dir, color=Color, scale=Scale, length=L }) ->
    if I >= L -> ok;
       true ->
	    Color1 = scale(Color, Scale/(I*I)),
	    case Dir of
		-1 ->
		    epx:pixmap_put_pixel(Pixmap, X0, 0, Color1),
		    X00 = wrap(X0+1,Width),
		    plot(Pixmap,I+1,X00,X1,Width,Sprite);
		1 ->
		    epx:pixmap_put_pixel(Pixmap, X0, 0, Color1),
		    X00 = wrap(X0-1,Width),
		    plot(Pixmap,I+1,X00,X1,Width,Sprite);
		0 ->
		    epx:pixmap_put_pixel(Pixmap, X0, 0, Color1),
		    epx:pixmap_put_pixel(Pixmap, X1, 0, Color1),
		    X00 = wrap(X0+1,Width),
		    X11 = wrap(X1-1,Width),
		    plot(Pixmap,I+2,X00,X11,Width,Sprite)
	    end
    end.

random_color() ->
    {random(100,255),random(100,255),random(100,255)}.

random(Min,Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    Min + uniform(Max-Min+1) - 1;
random(Min,Max) when is_number(Min), is_number(Max), Min =< Max ->
    Min + uniform(Max-Min).

uniform(Range) when is_integer(Range), Range >= 0 ->
    trunc(rand:uniform()*Range+1);
uniform(Range) when is_float(Range), Range >= 0 ->
    rand:uniform()*Range.

scale({R,G,B}, X) ->
    {trunc(R*X),trunc(G*X),trunc(B*X)}.

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
