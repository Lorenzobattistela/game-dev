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

void renderFloor(SDL_Renderer* renderer, const char* path)
{
    int tileWidth = 45;
    int tileHeight = 20;

    SDL_Surface* surface = IMG_Load(path);
    if (!surface) {
        SDL_Log("Failed to load image: %s", IMG_GetError());
        return;
    }

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    SDL_FreeSurface(surface);

    if (!texture) {
        SDL_Log("Failed to create texture: %s", SDL_GetError());
        return;
    }

    SDL_Rect srcRect = {
        .x = 16,
        .y = 200,
        .w = 45,
        .h = 20,
    };

    int tilesX = (WINDOW_WIDTH + tileWidth - 1) / tileWidth;
    int tilesY = (WINDOW_HEIGHT + tileHeight - 1) / tileHeight;

    for (int y = 0; y < tilesY; y++) {
        for (int x = 0; x < tilesX; x++) {
            SDL_Rect dstRect = {
                .x = x * tileWidth,
                .y = y * tileHeight,
                .w = tileWidth,
                .h = tileHeight
            };

            SDL_RenderCopy(renderer, texture, &srcRect, &dstRect);
        }
    }

    SDL_DestroyTexture(texture);
}


