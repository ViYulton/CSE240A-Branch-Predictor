//========================================================//
//  predictor.c                                           //
//  Source file for the Branch Predictor                  //
//                                                        //
//  Implement the various branch predictors below as      //
//  described in the README                               //
//========================================================//
#include <stdio.h>
#include <string.h>
#include "predictor.h"

//
// TODO:Student Information
//
const char *studentName = "NAME";
const char *studentID = "PID";
const char *email = "EMAIL";

//------------------------------------//
//      Predictor Configuration       //
//------------------------------------//

// Handy Global for use in output routines
const char *bpName[4] = {"Static", "Gshare",
                         "Tournament", "Custom"};

int ghistoryBits; // Number of bits used for Global History
int lhistoryBits; // Number of bits used for Local History
int pcIndexBits;  // Number of bits used for PC index
int bpType;       // Branch Prediction Type
int verbose;
int weightBits;

//------------------------------------//
//      Predictor Data Structures     //
//------------------------------------//

//
// TODO: Add your own Branch Predictor data structures here
//
int8_t *gCounter = NULL;
int *lHistory = NULL;
int8_t *lCounter = NULL;
int8_t *cCounter = NULL;
int ghistoryMask;
int lhistoryMask;
int pcMask;
int ghistory;

int **weight = NULL;
int weight_max;
int weight_min;
int theta;
//------------------------------------//
//        Predictor Functions         //
//------------------------------------//

// Initialize the predictor
//
void init_predictor()
{
  //
  // TODO: Initialize Branch Predictor Data Structures
  //
  switch (bpType)
  {
  case GSHARE:
    // memset(gCounter, 0, sizeof(gCounter));
    gCounter = calloc(1 << ghistoryBits, sizeof(int8_t));
    ghistoryMask = (1 << ghistoryBits) - 1;
    ghistory = 0;
    break;
  case TOURNAMENT:
    gCounter = calloc(1 << ghistoryBits, sizeof(int8_t));
    cCounter = calloc(1 << ghistoryBits, sizeof(int8_t));
    ghistoryMask = (1 << ghistoryBits) - 1;
    ghistory = 0;
    lHistory = calloc(1 << pcIndexBits, sizeof(int));
    pcMask = (1 << pcIndexBits) - 1;
    lCounter = calloc(1 << lhistoryBits, sizeof(int8_t));
    lhistoryMask = (1 << lhistoryBits) - 1;
    break;
  case CUSTOM:
    ghistoryMask = (1 << ghistoryBits) - 1;
    ghistory = 0;
    pcMask = (1 << pcIndexBits) - 1;
    weight = calloc(1 << pcIndexBits, sizeof(int *));
    for (int i = 0; i <= pcMask; i++)
    {
      weight[i] = calloc(ghistoryBits + 1, sizeof(int));
    }
    weight_max = (1 << (weightBits - 1)) - 1;
    weight_min = -weight_max - 1;
    theta = 1.93 * ghistoryBits + 14;
    break;
  default:
    break;
  }
}

// Make a prediction for conditional branch instruction at PC 'pc'
// Returning TAKEN indicates a prediction of taken; returning NOTTAKEN
// indicates a prediction of not taken
//
uint8_t
make_prediction(uint32_t pc)
{
  //
  // TODO: Implement prediction scheme
  //
  // Make a prediction based on the bpType
  // printf("0x%x\n", pc); //
  switch (bpType)
  {
  case STATIC:
    return TAKEN;
  case GSHARE:
  {
    int index = (pc ^ ghistory) & ghistoryMask;
    int8_t entry = gCounter[index];
    if (entry < 2)
    {
      return NOTTAKEN;
    }
    else
    {
      return TAKEN;
    }
  }
  case TOURNAMENT:
  {
    int choice = cCounter[ghistory];
    if (choice < 2)
    { // Simple BHT
      int8_t entry = gCounter[ghistory];
      if (entry < 2)
      {
        return NOTTAKEN;
      }
      else
      {
        return TAKEN;
      }
    }
    else
    { // Correlated predictor
      int history = lHistory[pc & pcMask];
      int8_t entry = lCounter[history];
      if (entry < 2)
      {
        return NOTTAKEN;
      }
      else
      {
        return TAKEN;
      }
    }
    break;
  }
  case CUSTOM:
  {
    int index = pc & pcMask;
    int y_out = weight[index][ghistoryBits];
    for (int i = 0; i < ghistoryBits; i++)
    {
      if (ghistory & (1 << i))
      {
        y_out += weight[index][i];
      }
      else
      {
        y_out -= weight[index][i];
      }
    }
    if (y_out >= 0)
    {
      return TAKEN;
    }
    else
    {
      return NOTTAKEN;
    }
  }
  default:
    break;
  }

  // If there is not a compatable bpType then return NOTTAKEN
  return NOTTAKEN;
}

// Train the predictor the last executed branch at PC 'pc' and with
// outcome 'outcome' (true indicates that the branch was taken, false
// indicates that the branch was not taken)
//
void train_predictor(uint32_t pc, uint8_t outcome)
{
  //
  // TODO: Implement Predictor training
  //
  switch (bpType)
  {
  case GSHARE:
  {
    int index = (pc ^ ghistory) & ghistoryMask;
    int8_t *entry = &gCounter[index];
    if (outcome)
    {
      *entry = min(3, *entry + 1);
    }
    else
    {
      *entry = max(0, *entry - 1);
    }
    ghistory = ((ghistory << 1) | outcome) & ghistoryMask;
    break;
  }
  case TOURNAMENT:
  {
    // Train global prediction
    int8_t *gEntry = &gCounter[ghistory];
    int p0 = *gEntry >= 2;
    if (outcome)
    {
      *gEntry = min(3, *gEntry + 1);
    }
    else
    {
      *gEntry = max(0, *gEntry - 1);
    }
    // Train local prediction
    int *history = &lHistory[pc & pcMask];
    int8_t *lEntry = &lCounter[*history];
    int p1 = *lEntry >= 2;
    if (outcome)
    {
      *lEntry = min(3, *lEntry + 1);
    }
    else
    {
      *lEntry = max(0, *lEntry - 1);
    }
    *history = ((*history << 1) | outcome) & lhistoryMask;
    // Train choice prediction
    int8_t *choice = &cCounter[ghistory];
    if (p0 == outcome && p1 != outcome)
    {
      *choice = max(0, *choice - 1);
    }
    else if (p0 != outcome && p1 == outcome)
    {
      *choice = min(3, *choice + 1);
    }
    ghistory = ((ghistory << 1) | outcome) & ghistoryMask;
    break;
  }
  case CUSTOM:
  {
    int index = pc & pcMask;
    int y_out = weight[index][ghistoryBits];
    for (int i = 0; i < ghistoryBits; i++)
    {
      if (ghistory & (1 << i))
      {
        y_out += weight[index][i];
      }
      else
      {
        y_out -= weight[index][i];
      }
    }
    if ((y_out >= 0) != outcome || abs(y_out) <= theta)
    {
      weight[index][ghistoryBits] += outcome == TAKEN ? 1 : -1;
      for (int i = 0; i < ghistoryBits; i++)
      {
        weight[index][i] += outcome == ((ghistory >> i) & 1) ? 1 : -1;
        weight[index][i] = min(weight_max, max(weight_min, weight[index][i]));
      }
    }
    ghistory = ((ghistory << 1) | outcome) & ghistoryMask;
  }
  default:
    break;
  }
}
