## ***

Hang Glider (hang\_glider)


# Project Contents

#### Code

- hang\_glider.nes\*

- Main\_menu.fab

- resources.fab

- sprites.fab

- Title.nam\*

- cliff.fab

- game.fab

- hang\_glider.cfg


#### Audio

- Music.txt\*


#### Graphics

- Plane.png

- title.png


#### Resource Project Files

- Music.ftm

\*Files which are automatically generated


## ***

# Game.fab

### Overview

This file handles the core game mechanics including player movement, game state management, sound and palette settings.


### Constants

- NUM\_DIR, MAX\_DIR, MID\_DIR, MIN\_DIR: Directional constants for game logic.

- PLAYER\_Y: Fixed y-axis position of the player.

- dead\_color: Color used to indicate player's defeat.


### Arrays

- game\_palette: Specifies the color palette for game elements.


### Variables

- px, py\_tile: Player's position coordinates.

- pscroll, pspeed: Scrolling position and speed.

- pscroll\_nt: Scrolling NT (name table) toggle.

- pdir: Current direction of the player.

- noise\_vol, held\_timer: Volume for sound effects and timer for held actions.

- rotated, pdead: Boolean states for player rotation and death.


### Functions

- game\_nmi(): Manages frame updates and visual/sound effects.

- move\_player(): Updates player’s movement based on inputs.

- collide\_player(): Checks and handles collisions.

- inc\_score(): Increments the game score and updates game speed.

- load\_level(): Loads level data into the game.

- mode game(): Main game loop handling input and game updates.


## Detailed breakdown of main loops

## nmi game\_nmi()

### Purpose

This function handles Non-Maskable Interrupts (NMI) which are crucial for performing essential tasks that need to occur during each frame of the game, such as updating the graphics and sound based on the game state.


### Functionality

1. Palette and Cliff Updates: It begins by uploading the current palette and cliff configurations to the Picture Processing Unit (PPU), ensuring that the visual elements are updated according to the game state.

2. Rotation and Death Check: If the player has rotated or isn't dead, the function resets specific sound channels and adjusts the volume and pitch based on the player's current direction (`pdir`).

3. Sound Adjustment: Adjusts the noise channel settings to decrease the noise volume gradually if the sound is active.

4. Gamepad Polling and Screen Updates:

   - Polls the gamepad to update player controls.

   - Updates the PPU status to manage screen scrolling.

   - Sets the PPU mask and control to ensure that graphics display correctly, handling screen rendering details like sprite visibility and background clipping.

5. Loop Termination Conditions: Checks conditions to continue the game loop or update the game mode, particularly managing game over scenarios or transitions.


### Key Operations

- ppu\_upload\_palette(): Updates the color palette on the PPU.

- ppu\_upload\_cliff(): Uploads cliff data to the PPU.

- Polling Pads and Updating PPU Registers: Ensures that input and visual outputs are synchronized with the game state.


## mode game()

### Purpose

The `mode game()` function serves as the main game loop where most of the gameplay mechanics are processed.


### Functionality

1. Initialization:

   - Initializes game settings like PPU control and mask settings to their default states.

   - Starts the background music for the game mode.

2. Game Loop Execution:

   - Continuously checks for player input.

   - Updates gameplay mechanics based on interactions and game rules (such as moving the player or checking for collisions).

   - Handles the game state transitions (for example, switching to a different mode if the player dies).

3. Sprite and Level Management:

   - Prepares sprite data for rendering.

   - Calls `nmi game_nmi()` repeatedly within the loop to handle frame-specific updates.

   - Loads and updates level components as needed.

4. Exit Conditions:

   - Detects when the player presses the start button to pause or resume.

   - Manages transitions between different game modes or states, like moving from gameplay to a pause menu or ending the game.


### Key Operations

- update\_pads(): Updates the state of the game controls.

- move\_player(), collide\_player(): Manage player movement and interactions with the game world.

- prepare\_sprites(): Prepares sprite data for rendering each frame.

***


# Main\_Menu.fab

### Overview

This file sets up the main menu, managing its visual presentation and interactions.


### Arrays

- main\_menu\_palette: Defines the color palette for the main menu.


### Data

- main\_menu\_nt: Title screen data, loaded from external files.


### Functions

- main\_menu\_nmi(): Sets up the display for the main menu.

- mode main(): Main menu loop, checking for player inputs to start the game.

***


# Resources.fab

### Overview

Manages external assets like graphics and audio required by the game.


### Graphics Resources

- plane.png, title.png: External files for character and title graphics.


### Audio Resources

- music.txt: Defines music tracks.

- puf1\_sfx: References to sound effects used throughout the game.

***


# Sprites.fab

### Overview

Handles the configuration and management of in-game sprites.


### Functions

- prepare\_sprites(): Prepares and positions sprites based on game state.


### Arrays

- plane\_patterns: Contains sprite pattern indices for animation frames.

***


# Cliff.fab

### Overview

This file manages the dynamic cliff elements within the game levels, influencing gameplay and visual representation.


### Constants

- SCROLL\_HEIGHT: Height at which cliffs reset.


### Structures

- Cliff: Contains start and end points for cliff gaps.


### Variables

- cliff\_hole, cliff\_midpoint, cliff\_index: Manage the positioning and dynamics of cliffs.

- cliffs: Array of `Cliff` structures representing the cliff segments in the game.


### Functions

- update\_cliff(): Updates cliff positions and dynamics.

- cliff\_ppuaddr\_table(): Maps `cliff_y` positions to PPU addresses.

- ppu\_upload\_cliff(): Uploads cliff data to the PPU based on the current game state.
