# Rubik's Cube auto-training

After creating the Rubik's cube visualization and solving application I though it would be fun to try to reseach machine-learning approach. I used the simplest Monte Carlo algorithm, in which we do random moves until we reach desired state. 
This would not be possible for the entire solving process as the 3x3 (or even 2x2) cube hs too many possible states. Instead I divided the training process into small stages, similar to LBL (layer-by-layer).

## Findings for 2x2 cube

This was fun and quite easy - the random approach worked quite well for 3-stage LBL solving process. I had to experiment with different training parameters, find subtle bugs etc. Ultimately solving works really well.

## Findings for 3x3 cube

## Code

I love scala so I wanted to start with pure functional style, nice abstraction layers and so on. Unfortunatelly this was not too performant so I had to break introduce several _mutabilities_ in order to speed up the learning process. 
This lead to hyprid approach where some classes have mutable state, some don't and it's a little bit of a mess. But at least it works.
