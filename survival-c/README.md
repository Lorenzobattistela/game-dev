# Survival  

This is a simple 2d pixeled survival game in C using SDL2.


## TODOs and Ideas

For now im working on rendering an enemy. Im thinking about having a structure like for the player. This because ill have to keep track of each enemy animation frame order, as well as data such as life, damage, speed, etc. So i should probably hold an Enemy struct and get this data there. Than I can have a list of frames just like for the character (there are 3 frames per direction) and just get the next frame accordingly.

The enemies movement should be done by calculating the distance from the player. So basically the enemies should always be calculating the fastest path to reach the player and walk towards this goal. When an enemy is killed, it stops rendering.

So basically we should have a list of "alive enemies" or so, and render only the ones not marked as dead.

The animation of the enemies should be done together with the fastest path calculation, since the animation depends on it (we dont have key events for enemies). But the fastest path will probably translate into points (x, y) and then we can know how to animate from there.

I also want to implement a clock to sinalize levels. Since its a survival, you have to survive for N seconds maybe? Thats the idea. 

Another TODO is the "powerup" for the player. I have not loaded the sprites for this yet and im not sure how ill activate it, but it should be as simple as adding more fields to my sprites ENUM and rendering accordingly when some key is pressed. Although im not sure how ill do about the skillshot yet. 
