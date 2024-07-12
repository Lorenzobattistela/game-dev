#ifndef GAME_H
#define GAME_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define WINDOW_WIDTH 950
#define WINDOW_HEIGHT 800
#define SPRITE_WIDTH 26
#define SPRITE_HEIGHT 36
#define FRAME_ROWS 5
#define FRAME_COLS 3

#define ANIMATION_FRAMES FRAME_ROWS * FRAME_COLS

#define ENEMY_WIDTH 36
#define ENEMY_HEIGHT 36
#define ENEMY_COLS 3
#define ENEMY_ROWS 4
#define ENEMY_ANIMATION_FRAMES ENEMY_COLS * ENEMY_ROWS
#define NUM_ENEMIES 7


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
  int life;
  int speed;
  bool dead;
} Player;

typedef struct {
  Position position;
  int life;
  int damage;
  int speed;
  bool dead;
  frames_t currentFrame;
} Enemy;

typedef struct {
  Position position;
  float width;
  float height;
} Object;

bool enemyHit(Player *p, Enemy *e);
bool hitAnyEnemy(Player *p, Enemy **enemies);
Enemy *createEnemyAtRandomPos(int life, int damage, int speed);

// this creates EQUAL enemies, same speed, damage and life
Enemy** createEnemies(int numEnemies, int life, int damage, int speed);

// here we calculate the enemy next step based on player position
// for example: player at: (0, 0) enemy at: (10, 4);
void updateEnemyPosition(Player *p, Enemy *e);
void updateEnemiesPosition(Player *p, Enemy **enemies);

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
