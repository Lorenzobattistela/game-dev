#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <stdio.h>
#include <stdbool.h>
#include "game.h"

#define SPRITE_WIDTH 26
#define SPRITE_HEIGHT 36
#define FRAME_ROWS 5
#define FRAME_COLS 3
#define ANIMATION_FRAMES FRAME_ROWS * FRAME_COLS
#define SPEED 2
#define FPS 60
#define FRAME_DELAY 1000 / FPS

// frames 0, 1, 2 horizontally 

SDL_Rect spriteClips[ANIMATION_FRAMES];
int frameCounter = 0;
int frameTime;
Uint32 frameStart;
Uint32 lastFrameTime = 0;
Uint32 lastPositionUpdateTime = 0;
const int ANIMATION_SPEED = 100;
const int POSITION_UPDATE_SPEED = 20;

typedef enum {
  QUIT,
  RUNNING,
  WON,
  LOST
} game_state_t;

typedef enum {
  W,
  A,
  S,
  D
} controls_t;

typedef enum {
  FRONT_WALK_RIGHT_HAND,
  FRONT_STANDING,
  FRONT_WALK_LEFT_HAND,
  LEFT_WALK_RIGHT_HAND,
  LEFT_WALK_STANDING,
  LEFT_WALK_LEFT_HAND,
  RIGHT_WALK_RIGHT_HAND,
  RIGHT_WALK_STANDING,
  RIGHT_WALK_LEFT_HAND,
  BACK_WALK_RIGHT_HAND,
  BACK_WALK_STANDING,
  BACK_WALK_LEFT_HAND,
} frames_t;

frames_t currentFrame = FRONT_STANDING;

typedef enum {
  FRONT,
  LEFT,
  RIGHT,
  BACK
} frame_type_t;

typedef struct {
  frame_type_t frame_type;
  frames_t frames[FRAME_COLS];
  frames_t next_frame;
} Frames;

Frames front = { .frame_type = FRONT, .frames = { FRONT_WALK_RIGHT_HAND, FRONT_STANDING, FRONT_WALK_LEFT_HAND }, .next_frame = FRONT_WALK_LEFT_HAND };
Frames left = { .frame_type = LEFT, .frames = { LEFT_WALK_RIGHT_HAND, LEFT_WALK_STANDING, LEFT_WALK_LEFT_HAND }, .next_frame = LEFT_WALK_LEFT_HAND };
Frames right = { .frame_type = RIGHT, .frames = { RIGHT_WALK_RIGHT_HAND, RIGHT_WALK_STANDING, RIGHT_WALK_LEFT_HAND }, .next_frame = RIGHT_WALK_LEFT_HAND };
Frames back = { .frame_type = BACK, .frames = { BACK_WALK_RIGHT_HAND, BACK_WALK_STANDING, BACK_WALK_LEFT_HAND }, .next_frame = BACK_WALK_LEFT_HAND };


typedef struct {
  float x;
  float y;
} Position;

typedef struct {
  Position position;
} Player;

int updateFrontFrame(frames_t current) {
  int curr = (int)current;
  if (curr >= FRONT_WALK_LEFT_HAND) {
    return FRONT_WALK_RIGHT_HAND;
  }
  return curr + 1;
}

int updateLeftFrame(frames_t current) {
  int curr = (int)current;
  if (curr >= LEFT_WALK_LEFT_HAND) {
    return LEFT_WALK_RIGHT_HAND;
  }
  return curr + 1;
}

int updateRightFrame(frames_t current) {
  int curr = (int)current;
  if (curr >= RIGHT_WALK_LEFT_HAND) {
    return RIGHT_WALK_RIGHT_HAND;
  }
  return curr + 1;
}

int updateBackFrame(frames_t current) {
  int curr = (int)current;
  if (curr >= BACK_WALK_LEFT_HAND) {
    return BACK_WALK_RIGHT_HAND;
  }
  return curr + 1;
}


frames_t getNextFrame(controls_t control) {
  frames_t f;
  switch (control) {
    case W:
      f = back.next_frame;
      back.next_frame = updateBackFrame(f);
      return f;
    case S:
      f = front.next_frame;
      front.next_frame = updateFrontFrame(f);
      return f;
    case A:
      f = left.next_frame;
      left.next_frame = updateLeftFrame(f);
      return f;
    case D:
      f = right.next_frame;
      right.next_frame = updateRightFrame(f);
      return f;
  }
  return FRONT_STANDING;
}

bool controls[4] = {false, false, false, false};

bool isControlPressed(controls_t control) {
  return controls[control];
}

void pressControl(controls_t control) {
  controls[control] = true;
}

void releaseControl(controls_t control) {
  controls[control] = false;
}

void updatePlayerPosition(Player *p) {
    if (isControlPressed(W)) {
        p->position.y -= SPEED;
    }
    if (isControlPressed(S)) {
        p->position.y += SPEED;
    }
    if (isControlPressed(A)) {
        p->position.x -= SPEED;
    }
    if (isControlPressed(D)) {
        p->position.x += SPEED;
    }
}

void clampPlayerPosition(Player *p) {
    if (p->position.x < 0) p->position.x = 0;
    if (p->position.y < 0) p->position.y = 0;
    if (p->position.x > WINDOW_WIDTH - SPRITE_WIDTH) p->position.x = WINDOW_WIDTH - SPRITE_WIDTH;
    if (p->position.y > WINDOW_HEIGHT - SPRITE_HEIGHT) p->position.y = WINDOW_HEIGHT - SPRITE_HEIGHT;
}

game_state_t state = RUNNING;

SDL_Texture* loadSpritesheet(SDL_Renderer* renderer, const char* path) {
    SDL_Surface* loadedSurface = IMG_Load(path);
    if (loadedSurface == NULL) {
        printf("Unable to load image %s! SDL_image Error: %s\n", path, IMG_GetError());
        return NULL;
    }

    SDL_Texture* spritesheet = SDL_CreateTextureFromSurface(renderer, loadedSurface);
    if (spritesheet == NULL) {
        printf("Unable to create texture from %s! SDL Error: %s\n", path, SDL_GetError());
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
    SDL_RenderCopy(renderer, spritesheet, &spriteClips[currentFrame], &renderQuad);
}

int main(int argc, char* argv[]) {
    SDL_Window* window = NULL;
    SDL_Renderer* renderer = NULL;
    SDL_Event event;

    Player *player = malloc(sizeof(Player));
    Position initialPosition = { .x = 0, .y = 0 };

    if (player == NULL) {
      printf("ERROR on mallocing player.\n");
      return EXIT_FAILURE;
    }
    player->position = initialPosition;

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }

    window = SDL_CreateWindow("Survival", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
    if (window == NULL) {
        printf("Window could not be created! SDL_Error: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (renderer == NULL) {
        printf("Renderer could not be created! SDL_Error: %s\n", SDL_GetError());
        return EXIT_FAILURE;
    }

    SDL_Texture *characterSpritesheet = loadSpritesheet(renderer, "./assets/bardo.png");

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
          updatePlayerPosition(player);
          clampPlayerPosition(player);
          lastPositionUpdateTime = currentTime;
        }

        if (currentTime - lastFrameTime > ANIMATION_SPEED) {
          if (isControlPressed(W)) currentFrame = getNextFrame(W);
          else if (isControlPressed(S)) currentFrame = getNextFrame(S);
          else if (isControlPressed(A)) currentFrame = getNextFrame(A);
          else if (isControlPressed(D)) currentFrame = getNextFrame(D);
          else currentFrame = FRONT_STANDING; // Reset to standing if no key is pressed
          lastFrameTime = currentTime;
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);
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
