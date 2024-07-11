#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <stdio.h>
#include <stdbool.h>
#include "game.h"
#include "error.h"

bool controls[4] = {false, false, false, false};

frames_t currentFrame = FRONT_STANDING;

Frames front = { .frame_type = FRONT, .frames = { FRONT_WALK_RIGHT_HAND, FRONT_STANDING, FRONT_WALK_LEFT_HAND }, .next_frame = FRONT_WALK_LEFT_HAND };
Frames left = { .frame_type = LEFT, .frames = { LEFT_WALK_RIGHT_HAND, LEFT_WALK_STANDING, LEFT_WALK_LEFT_HAND }, .next_frame = LEFT_WALK_LEFT_HAND };
Frames right = { .frame_type = RIGHT, .frames = { RIGHT_WALK_RIGHT_HAND, RIGHT_WALK_STANDING, RIGHT_WALK_LEFT_HAND }, .next_frame = RIGHT_WALK_LEFT_HAND };
Frames back = { .frame_type = BACK, .frames = { BACK_WALK_RIGHT_HAND, BACK_WALK_STANDING, BACK_WALK_LEFT_HAND }, .next_frame = BACK_WALK_LEFT_HAND };

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

void updateNextFrame(frames_t f) {
  currentFrame = f;
}

frames_t getCurrentFrame() {
  return currentFrame;
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

bool hit(Player *p, Object *o) {
  if (p->position.x < o->position.x + o->width &&
      p->position.x + SPRITE_WIDTH > o->position.x &&
      p->position.y < o->position.y + o->height &&
      p->position.y + SPRITE_HEIGHT > o->position.y) {
    return true;
  }
  return false; 
}

void clampObjectCollision(Player *p, Object *o) {
    if (hit(p, o)) {
        // Calculate the overlap on each axis
        float overlapX = (p->position.x < o->position.x + o->width / 2) ?
            (p->position.x + SPRITE_WIDTH) - o->position.x :
            (o->position.x + o->width) - p->position.x;
        
        float overlapY = (p->position.y < o->position.y + o->height / 2) ?
            (p->position.y + SPRITE_HEIGHT) - o->position.y :
            (o->position.y + o->height) - p->position.y;

        // Determine which axis has the smaller overlap and adjust only that axis
        if (overlapX < overlapY) {
            if (p->position.x < o->position.x + o->width / 2) {
                p->position.x = o->position.x - SPRITE_WIDTH;
            } else {
                p->position.x = o->position.x + o->width;
            }
        } else {
            if (p->position.y < o->position.y + o->height / 2) {
                p->position.y = o->position.y - SPRITE_HEIGHT;
            } else {
                p->position.y = o->position.y + o->height;
            }
        }
    }
}

