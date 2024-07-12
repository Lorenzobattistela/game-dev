#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <stdio.h>
#include <stdbool.h>
#include "game.h"
#include "rendering.h"
#include "error.h"

int frameCounter = 0;
int frameTime;
Uint32 frameStart;
Uint32 lastFrameTime = 0;
Uint32 lastPositionUpdateTime = 0;
const int ANIMATION_SPEED = 100;
const int POSITION_UPDATE_SPEED = 20;
game_state_t state = RUNNING;

int main(int argc, char* argv[]) {
    SDL_Window* window = NULL;
    SDL_Renderer* renderer = NULL;
    SDL_Event event;

    Enemy **enemies = createEnemies(NUM_ENEMIES, 100, 100, 1);

    Player *player = malloc(sizeof(Player));
    Position initialPosition = { .x = 0, .y = 0 };
 
    if (player == NULL) {
      error("failed to malloc player structure.");
    }
    player->position = initialPosition;

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        error(SDL_GetError());
    }

    window = SDL_CreateWindow (
        "Survival", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 
        WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN
    );

    if (window == NULL) {
      error(SDL_GetError());
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (renderer == NULL) {
      error(SDL_GetError());
    }

    SDL_Texture *characterSpritesheet = loadSpritesheet(renderer, "./assets/bardo.png");
    SDL_Texture *enemySpritesheet = loadEnemySpritesheet(renderer, "./assets/reaper.png");

    if (TTF_Init() == -1) {
      error("SDL_ttf could not initialize!");
    }

    
    while (state != QUIT) {
        frameStart = SDL_GetTicks();
 
        while (SDL_PollEvent(&event) != 0) {
          switch(event.type) {
            case SDL_QUIT:
              state = QUIT;
              break; 
            
              case SDL_KEYDOWN:
                switch(event.key.keysym.sym) {
                    case SDLK_w:
                        pressControl(W); 
                        break;

                    case SDLK_a:
                        pressControl(A);
                        break;

                    case SDLK_s:
                        pressControl(S);
                        break;

                    case SDLK_d:
                        pressControl(D);
                        break;
                }
                break;
            
            case SDL_KEYUP:
                switch(event.key.keysym.sym) {
                    case SDLK_w:
                        releaseControl(W);
                        break;

                    case SDLK_a:
                        releaseControl(A);
                        break;

                    case SDLK_s:
                        releaseControl(S);
                        break;

                    case SDLK_d:
                        releaseControl(D);
                        break;
                }
                break;
          }
        }

        Uint32 currentTime = SDL_GetTicks();
        if (currentTime - lastPositionUpdateTime > POSITION_UPDATE_SPEED) {
          if (hitAnyEnemy(player, enemies)) {
            // handle life here
            displayGameOver(renderer);
            state = QUIT;
          }
          updatePlayerPosition(player);
          updateEnemiesPosition(player, enemies);
          clampPlayerPosition(player);
          lastPositionUpdateTime = currentTime;
        }

        if (currentTime - lastFrameTime > ANIMATION_SPEED) {
          if (isControlPressed(W)) updateNextFrame(getNextFrame(W));
          else if (isControlPressed(S)) updateNextFrame(getNextFrame(S));
          else if (isControlPressed(A)) updateNextFrame(getNextFrame(A));
          else if (isControlPressed(D)) updateNextFrame(getNextFrame(D));
          else updateNextFrame(FRONT_STANDING);
          lastFrameTime = currentTime;
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);
        renderFloor(renderer, "./assets/darkdimension.png");
        renderEnemies(renderer, enemySpritesheet, enemies);
        renderCharacter(renderer, characterSpritesheet, player->position.x, player->position.y);
        SDL_RenderPresent(renderer);

        frameTime = SDL_GetTicks() - frameStart;
        if (FRAME_DELAY > frameTime) {
          SDL_Delay(FRAME_DELAY - frameTime);
        }
    }

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
    return EXIT_SUCCESS;
}
