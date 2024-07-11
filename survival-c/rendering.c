#include "rendering.h"
#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include "error.h"

void renderObject(SDL_Renderer* renderer, Object* obj) {
    SDL_Rect rect = {obj->position.x, obj->position.y, obj->width, obj->height};
    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);  // Red color
    SDL_RenderFillRect(renderer, &rect);
}

SDL_Rect spriteClips[ANIMATION_FRAMES];

SDL_Texture* loadSpritesheet(SDL_Renderer* renderer, const char* path) {
    SDL_Surface* loadedSurface = IMG_Load(path);
    if (loadedSurface == NULL) {
      error(IMG_GetError());
    }

    SDL_Texture* spritesheet = SDL_CreateTextureFromSurface(renderer, loadedSurface);
    if (spritesheet == NULL) {
      error(SDL_GetError());
    }

    SDL_FreeSurface(loadedSurface);

    for (int i = 0; i < ANIMATION_FRAMES; i++) {
        int row = i / FRAME_COLS;
        int col = i % FRAME_COLS;
        
        spriteClips[i].x = col * SPRITE_WIDTH;
        spriteClips[i].y = row * SPRITE_HEIGHT;
        spriteClips[i].w = SPRITE_WIDTH;
        spriteClips[i].h = SPRITE_HEIGHT;
    }

    return spritesheet;
}

void renderCharacter(SDL_Renderer* renderer, SDL_Texture* spritesheet, int x, int y) {
    SDL_Rect renderQuad = { x, y, SPRITE_WIDTH, SPRITE_HEIGHT };
    frames_t currentFrame = getCurrentFrame();
    SDL_RenderCopy(renderer, spritesheet, &spriteClips[currentFrame], &renderQuad);
}






