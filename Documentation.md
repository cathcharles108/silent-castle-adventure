# Original documentation submitted for the Functional Adventure project of MPCS 51400: Functional Programming

## Name: Catherine Charles

## Date: December 9, 2022

## Acknowledgments

Many thanks to Matt Teichman, Jacob Bennett, and the rest of the MPCS 51400 teaching staff who have guided me throughout this project.

## Instructions

### New feature: To-do list

The implemented feature is a to-do list, where the player can access by typing `todo` in the console.
The to-do list is implemented by way of listing all tasks, then slowly checking them off as the player finishes them.
The to-do list also functions as a level up, as certain aspects of the game can only be unlocked after completing certain parts of the to-do list.
For example, the player has to take the scroll before being able to find the crown. A full list of levels can be found below.

### New feature: leveling up based on to-do list

While not explicitly organized as such, the implementation of the to-do list implies that there are certain areas of the game that can only be unlocked after finishing certain tasks.
Details are as follows:
1. The player needs to go up to the tower before a new to-do item spawns to take the scroll.
2. The player needs to have taken the scroll before being able to find the crown, tome, key, or sceptre.
3. The player needs to have opened the chest before the magic paintbrush appears.
4. The player needs to have found the magic paintbrush before the command `paint` registers. Otherwise, the game will not understand the command.
5. The player needs to have revealed the hidden door before the secret room (garden and outside) appear.

### New feature: spawning and disappearing items

As mentioned earlier, a lot of items are only unlocked after the player does something. The full list of spawned items are found above.
As a converse, items can also disappear completely from the game. This is referred to in the game as either 'put in pocket' or 'dissolves into golden light'. Thus, the item not being found in the player's inventory is not a bug--rather, it is intended that way.
This feature is designed so that it does not confuse the player.
Such items include the following:
1. After taking the scroll, the scroll disappears from the game altogether. It is not found in the player's inventory. Rather, the act of taking the scroll is meant to activate the `scroll` command, as well as progress further in the game. It is not designed to be dropped by or taken around with the player.
2. After dropping the key in the room with the chest (thereby unlocking the chest), the key disappears. This is so that it is clear to the player that the key has done its purpose and is no longer of any use to the player--also to avoid instances like the player attempting to unlock the door using the key.
3. After using the paintbrush to reveal the door, the paintbrush 'dissolves into golden light'. This is in line with the specifics of the project, where the secret item needed to be a one-use item.

### New feature: extra commands added to enhance the storyline

The project added a few more commands that the player could perform to enhance gameplay. This includes the following:
1. `story` lets the player read the main intro storyline to the game again.
2. `scroll` is only unlocked after the player takes the scroll, and lets the player read the contents of the scroll again.
3. `instructions` allows the player to read through the list of available moves in the game.
4. `paint` is only unlocked after the player finds the paintbrush, and lets the player reveal the hidden door.
5. `todo` opens up the player's to-do list with tasks they must complete to advance in the game.

### Required secret room feature: paint to reveal door

The implementation of this feature is inspired by Barbie as Rapunzel, where the door is only revealed after the player paints the right wall in the right room.
Any attempts of painting the wrong wall or a wall with an exit will result in the failure of the paint action.
The `paint` command is also designed such that it will only work successfully once, then the paintbrush will disappear afterwards. This is in line with the spec of the project.
Once the player reveals the door, the 'secret room' is a garden that also leads to the outside. That is, both the garden and the outside were inaccessible by the player before the door is revealed.

### Winning the game

Here is the solution to winning this game:
1. Go north and north to the tower
    - Associated to-do item: Head up to the tower
2. Take the scroll
    - Associated to-do item: Take the odd-looking scroll on the parapet of the tower
3. Go south to the library and take the tome
    - Associated to-do item: Find the book of knowledge
4. Take the key
    - Associated to-do item: Take the key
5. Go south and then east to the throne room, then take the crown
    - Associated to-do item: Find the crown of the kingdom
6. Go west and then south to the bedroom, then drop the key
    - Associated to-do item: Bring key to room with the chest and put the key down
7. Take the magic paintbrush
    - Associated to-do item: Find your magic paintbrush
8. Go north to the Great Hall and paint the west wall
    - Associated to-do item: Use the paintbrush on the west wall of the Great Hall to reveal the door
9. Go west to the garden and take the sceptre
    - Associated to-do item: Find the sceptre of the kingdom
10. Make sure you are carrying the crown and the sceptre and go west to the outside to win the game
    - Associated to-do item: Bring the crown and sceptre with you and leave the castle

## New module added: Tasks

A new module called Tasks was added to the project, and Tasks contains the full list of to-do items a player could get in a game.
This module outlines all the things the player needs to do before winning the game.
It is in a new module for the same reasons why Direction and Item are their own modules. It makes it easier to reference the list of tasks by other modules during the development process.
