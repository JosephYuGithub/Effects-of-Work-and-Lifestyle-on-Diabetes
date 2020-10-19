---
title: "The Effects of Work and Lifestyle on Individuals with Diabetes"
author: "Group 12"
date: "2020-10-19"
output: pdf_document
---


## Title of your Report

# Chae Bae, Pritam Sinha, Chen Hao Yu
# October 18, 2020


## Abstract

Here is where you give a brief (one paragraph overview of your entire paper). This should include some background/introduction, some methodology, results and conclusions.

Out paper is designed to look at the effects of work and lifestyle on the age at which the recipients of the General Social Survey (GSS) are diagnosed with diabetes. Our model will look at a select few variables and attempt to create regression models based on these variables.

## Introduction

Here is where you should give insight into the setting and introduce the goal of the analysis. Here you can introduce ideas and basic concepts regarding the study setting and the potential model. Again, this is the introduction, so you should be explaining the importance of the work that is ahead and hopefully build some suspense for the reader. You can also highlight what will be included in the subsequent sections.

The tidyverse library is an extensive collection of different libraries used commonly in statistical work. In this study, we will be using primarily the dplyr library and the ggplot2 library, both part of the tidyverse collection. The dplyr library is a useful collection that allows us to clean the dataset. We can subset variables using the select function, create new columns using the mutate function, rename column names from their original titles in the GSS survey to more appropriate and descriptive names with the rename function, and group row operations using rowwise. The ggplot2 library allows us to map out our models with a variety of different options, in this study we will be only using a scatterplot using geom_point. $\\$

The janitor library


## Data

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)


library(janitor)
library(tidyverse)

```


```{r read data}

raw_health <- read_csv("AAsD8pYL_1.csv")

```


```{r}
health <- raw_health %>% select(CASEID, dvagegr, dvsex, dvperinc, dvbmimdf, a9cap, g2cap, g3cap, h2hrs, j3, k4, m17cap, m19) 


health <- health %>% clean_names() %>% 
    rename (case_id = caseid, age_first_diagnosed_with_diabetes = a9cap, sex = dvsex,  
            age = dvagegr, personal_income_12months = dvperinc, BMI = dvbmimdf, 
            height = g2cap, weight = g3cap, sleep_hours = h2hrs, 
            cigarettes_smoke_per_day= j3, average_drink_for_12_months = k4, 
            work_hours_per_week = m17cap, nightshift_frequency = m19)

#!!!not sure about this function



health <- health %>% 
  rowwise() %>% 
  mutate(age_first_diagnosed_with_diabetes = case_when(
      age_first_diagnosed_with_diabetes== 96 ~ as.numeric(NA),
      age_first_diagnosed_with_diabetes== 97 ~ as.numeric(NA),
      age_first_diagnosed_with_diabetes== 98 ~ as.numeric(NA),
      age_first_diagnosed_with_diabetes== 99 ~ as.numeric(NA),
      TRUE ~ as.numeric(age_first_diagnosed_with_diabetes)
))


health <- health %>% 
  rowwise() %>% 
  mutate(BMI = case_when(
      BMI== 99.9 ~ as.numeric(NA),
      TRUE ~ as.numeric(BMI)
))


health <- health %>% 
  rowwise() %>% 
  mutate(height = case_when(
      height== 998 ~ as.numeric(NA),
      height== 999 ~ as.numeric(NA),
      TRUE ~ as.numeric(height)
))


health <- health %>% 
  rowwise() %>% 
  mutate(weight = case_when(
      weight== 998 ~ as.numeric(NA),
      weight== 999 ~ as.numeric(NA),
      TRUE ~ as.numeric(weight)
))


health <- health %>% 
  rowwise() %>% 
  mutate(work_hours_per_week = case_when(
      work_hours_per_week== 97 ~ as.numeric(NA),
      work_hours_per_week== 99 ~ as.numeric(NA),
      TRUE ~ as.numeric(work_hours_per_week)
))



health <- health %>% 
  rowwise() %>% 
  mutate(sleep_hours = case_when(
      sleep_hours== 98 ~ as.numeric(NA),
      sleep_hours== 99 ~ as.numeric(NA),
      TRUE ~ as.numeric(sleep_hours)
))


health <- health %>% 
  rowwise() %>% 
  mutate(cigarettes_smoke_per_day = case_when(
      cigarettes_smoke_per_day== 97 ~ as.numeric(NA),
      cigarettes_smoke_per_day== 99 ~ as.numeric(NA),
      TRUE ~ as.numeric(cigarettes_smoke_per_day)
))

health <- na.omit(health)


health



```