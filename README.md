# Rubik's Cube auto-training

After creating the Rubik's cube visualization and solving application I though it would be fun to try to reseach machine-learning approach. I used the simplest Monte Carlo algorithm, in which we do random moves until we reach desired state. 
This would not be possible for the entire solving process as the 3x3 (or even 2x2) cube hs too many possible states. Instead I divided the training process into small stages, similar to LBL (layer-by-layer).

## Problem space

There is an enormous number of possible states for the 3x3 cube. This makes such a simple Monte Carlo approach impossible.
I knew it from the beginning, so I had to fin other way.

I already knew that I must divide the work into stages. So for each stage I _masked_ the state of irrelevant tiles.
For example when solving white cross we are only interested in 4 bottom edges (the ones with white), also with center tiles,
but they are not moving anyway. This reduces the problem space significantly.

## Findings for 2x2 cube

This was fun and quite easy - the random approach worked quite well for 3-stage LBL solving process. I had to experiment with different training parameters, find subtle bugs etc. Ultimately solving works really well.

## Findings for 3x3 cube

This was much more difficult. For white cross it still went well. For white corners
the was no way to find any random sequence that would lead to solving all white corners (at least in less than 100 moves).
So I divided it into substages, one per each corner.
I did the same for middle layer.

Unfortunately I got stuck on yellow cross state. The state space was too large to find any random solution. So I went different way.
I already have a code that solves the cube using LBL method. I generated 100k examples that solve 
the yellow layer (I also experimented with yellow cross only). I pretrained the q-states table with generated examples
so I had a starting point. I did the same for the upper layer (corners and edges).

Although this worked surprisingly well, it actually copies the moves from my former code (which in turn copies the moves from the online tutuorial).
Not exactly what I was hoping for but at least I finished the whole solving process somehow.

## Code

I love scala so I wanted to start with pure functional style, nice abstraction layers and so on. Unfortunatelly this was not too performant so I had to break introduce several _mutabilities_ in order to speed up the learning process. 
This lead to hyprid approach where some classes have mutable state, some don't and it's a little bit of a mess. But at least it works.
