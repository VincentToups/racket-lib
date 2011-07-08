Purely Function Procedural Content Generation
=============================================
With an Extended Turtle Monad
-----------------------------

[Last time][last-time] we constructed a state-like monad in Scheme for
an extended version of turtle graphics.  Unlike regular turtle
graphics, which feature a single turtle which can be moved around the
screen using simple commands, our extension supported "parallel
turtles."  That is, in regular turtle graphics we might tell our
turtle to turn left.  Our extended turtle can be told to turn left and
right at the same time.  Our system interprets such commands as
implicitly splitting our turtle, so that if we issue a subsequent
command, two turtles now proceed, one which turned left and one which
turned right.  For instance:

    (turtles-go 
      (mlet* m-turtles
        ((p (setl 'pos (point 150 150)))
         (new-pos (move-line 15))
         (h helicity-split)
         (dir (turn (/ pi 2)))
         (pos (move-line 15))
         (h helicity-split)
         (dir (turn (/ pi 2)))
         (pos (move-line 10))
         (h helicity-split)
         (dir (turn (/ pi 2)))
         (final-pos (move-line 5)))
       (m-return final-pos)))

Results in this image:

![TurtleExample](http://dl.dropbox.com/u/1076954/bifurcation-example.png)

If you didn't follow the last tutorial on monadic operation, read the
above code by looking at the right hand side of each of the
expressions in the list after `m-turtles`.  Each is a command that
also generates a value.  `move-line`, for instance, moves the position
of the turtle by the amount passed in and returns the final position.
The left hand side of each expression is a variable to which that
result is bound.  Conceptually, commands can return multiple values,
as, for instance, `helicity-split` does.  Helicity is a state variable
which controls which direction a turtle turns in when it receives a
turn command.  `helicity-split` sets helicity to both 1 and -1 at the
same time, and in all phrases subsequent to that line, `h` has _both
values_.  It isn't a list of both values.  It semantically has both
values simultaneously.  If this seems like wizardry to you, check out
the [previous post about monads][last-time].

So how do we read the above code?  Well, the first `setl` sets a
turtle-local variable, the `'pos`, to the value `(point 150 150)`.
Point is a struct.  We then use `move-line` to move and add a line to
the global list of things to draw.  `helicity-split` causes our first
turtle bifurcation, simultaneously setting `helicity` to plus and
minus one.  We then say `turn pi over 2` but each of our turtles
interprets this differently.  One turns to the left, the other to the
right.  We `draw-line` again, but this operation moves two turtles and
draws two lines.  We repeat this a few times for flavor.

Finally, we `m-return` the final position of the monad.  The `mlet*`
form creates a `procedure` which is waiting for a state-value.
`turtles-go` passes it that initial state value, extracts the elements
that are supposed to be drawn, pops up a window, and draws them.

This is a lot to take in if you haven't read the previous tutorial,
but if your comfortable just accepting this without completely getting
it, lets move to the next section.  If not, go [here][last-time].

Procedural Dungeon Design
-------------------------

I freaking love procedural content generation.  I spend a lot of my
day job doing the opposite (procedural pattern recognition) and both
involve this idea that you can teach a computer to do something people
ordinarily think computers can't do.  There is a nice wiki about the
subject: [The Procedural Content Generation Wiki][pcg-wiki], and it
expounds that there are at least two approaches to content generation:

1.   Ontogenic Generation, which attempts to replicate the final
result of system using heuristics, without simulating it.
2.   Simulation: Which simulates a process that produces the final
content.

If you think about turtle graphics, they generate content by
simulating the action of a turtle or a cursor moving around the page.
Images are synthesize by coming up with different rules for turtle
behavior and iterating or executing them.  You can certainly just tell
the turtle to draw a specific image, but its much more fun to
experiment with small amounts of code which produce interesting images
by iteration.

Today we're going to conscript our turtle to generate simple
dungeons.  In order to do this nicely, we need to decide how we're
going to represent the dungeon.  

Since we're just knocking about, our dungeon will just have two
features - rooms and hallways.  Since I wrote the last post, I
factored out a bunch of planar geometry code into its own library:

    (require utilities/planar-geometry)

Which now subserve drawing operations in the turtle monad. We're just
going to wrap up some of the structures from this library to represent
rooms and halls:

    (define room rectangle)
    (define hall line-segment)

Both `room` and `hall` take two points and pass them to `rectangle` or
`line-segment`.  These are constructor functions for data structures.
For a line-segment, the two points define the start and end of the
line.  For a rectangle, the points define opposite corners of the
rectangle/room.

We are going to let halls intersect naturally, but we don't want to
place rooms on top of rooms.  To make this process of testing slightly
more efficient, we'll place rooms and halls in their own
turtle global state variables.  So before we ask `turtles-go` to draw
everything, we should move these objects into the `draw-these` cue.
Future versions might have a more complex rendering process, so its
nice to separate things out now.  Here is that function:

    (define render-dungeon-simple
      (mlet* m-turtles
             ((rooms (getg-or 'rooms '()))
              (halls (getg-or 'halls '())))
             (setg 'draw-these (append rooms halls))))

The `mlet*` expression produces a monadic function which copies
`rooms` and `halls` from their global slots and puts them together
into `draw-these`.  Since rooms and slots are `planar-geometry`
primitives, the `turtles-monad` library knows how to draw them
already.

Ok, preliminaries out of the way, lets get to content generation.  How
shall we approach the turtle's behavior so that we get a nice
random-ish dungeon?

Here is the plan: 

1.   A single turtle should start in a central location, laying a
room.
2.   Maybe turn by pi over 2
3.   The turtle should now bifurcate by helicity. 
4.   Now the turtle should check the location 10 units in front of
it.  If the space is empty, it should create a room there, connect
it to the current room with a hall, and then move to the new room.  If
there is already a room there, sometimes it should create a new hall,
sometimes not.  If there isn't a room there and it can't create one
because it would overlap with a previous room, it should create a dead
end occasionally.
5.   Sometimes, a turtle should retire.
6.   Go to step 2, repeating N times.

By repeating steps 2-5 N times, we create tons of little turtles
carving out rooms and building halls.


  




   
           

* * *

[last-time]:http://dorophone.blogspot.com/2011/05/hyperturtle-monad-makes-pretty-pictures.html
[pcg-wiki]:http://pcg.wikidot.com/