#ifndef GAME_H
#define GAME_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define WINDOW_WIDTH 800
#define WINDOW_HEIGHT 600
#define SPRITE_WIDTH 26
#define SPRITE_HEIGHT 36
#define FRAME_ROWS 5
#define FRAME_COLS 3
#define ANIMATION_FRAMES FRAME_ROWS * FRAME_COLS
#define SPEED 2
#define FPS 60
#define FRAME_DELAY 1000 / FPS

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

typedef struct {
  float x;
  float y;
} Position;

typedef struct {
  Position position;
} Player;

typedef struct {
  Position position;
  float width;
  float height;
} Object;

bool hit(Player *p, Object *o);
void clampObjectCollision (Player *p, Object *o);

int updateFrontFrame(frames_t current);
int updateLeftFrame(frames_t current);
int updateRightFrame(frames_t current);
int updateBackFrame(frames_t current);

frames_t getNextFrame(controls_t control);
void updateNextFrame(frames_t f);
frames_t getCurrentFrame();
bool isControlPressed(controls_t control);
void pressControl(controls_t control);
void releaseControl(controls_t control);
void updatePlayerPosition(Player *p);
void clampPlayerPosition(Player *p);

#endif
