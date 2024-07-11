#ifndef RENDERING_H
#define RENDERING_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <stdio.h>
#include "game.h"

SDL_Texture* loadSpritesheet(SDL_Renderer* renderer, const char* path);
void renderCharacter(SDL_Renderer* renderer, SDL_Texture* spritesheet, int x, int y);
void renderObject(SDL_Renderer *renderer, Object *obj);


#endif
