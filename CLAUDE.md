# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

County Quizzer is a Delphi FireMonkey (FMX) cross-platform application that helps users learn US state counties through interactive quizzes. The app supports three quiz modes: recall (list all counties), multiple choice recognition, and spelling (unscramble county names). Data is stored in a SQLite database for easy maintenance and expansion.

## Build Commands

This is a Delphi project that uses the standard Delphi/RAD Studio build system:

- **Build Project**: Use RAD Studio IDE or MSBuild with `CountyQuizzer.dproj`
- **Debug Build**: Target Win64/Debug platform configuration
- **Release Build**: Target Win64/Release platform configuration  
- **Output Directory**: `Win64\Debug\` or `Win64\Release\`
- **Main Executable**: `CountyQuizzer.exe`

No special build scripts or external dependencies are required - this is a standard FMX application.

## Code Architecture

### Project Structure
- `CountyQuizzer.dpr` - Main program entry point
- `ufrmCountyQuizzerMain.pas/.fmx` - Main form implementation and UI layout
- `udmCountyData.pas/.dfm` - Data module for database access and data management
- `counties_data.sql` - SQL script to create and populate the SQLite database
- `counties.db` - SQLite database file (created by running the SQL script)

### Key Components

**Main Form (`TfrmStateCountyQuiz`)**:
- Manages quiz state and user interactions
- Contains all UI controls and event handlers
- Handles three distinct quiz modes with shared UI elements

**Data Module (`TdmCountyData`)**:
- `Connection: TFDConnection` - SQLite database connection using FireDAC
- `Query: TFDQuery` - Primary query object for database operations
- `LoadStates()` - Returns array of all states with their county data
- `GetStatesList()` - Returns array of state names only
- `GetCountiesForState()` - Returns counties for a specific state
- `GetRandomCountyFromOtherStates()` - Returns random county from states other than specified
- Database schema: `states` table (id, name, county_count) and `counties` table (id, state_id, name)
- Centralizes all database access logic

**Data Management (Main Form)**:
- `FStates: TDictionary<string, TStateInfo>` - Caches state and county data loaded from data module
- `TStateInfo` record contains state name, county count, and county array (defined in data module)
- Data loaded from data module in `InitializeStates()` method
- Currently includes data for Oregon, Washington, California, Texas, Florida, New York, and a test state

**Quiz Logic**:
- `TQuizMode` enum: `qmRecall`, `qmMultChoice`, `qmSpelling`
- `FShuffledCounties` array maintains randomized county order
- `TQuizStats` record tracks correct/total answers and calculates accuracy
- Fisher-Yates shuffle algorithm used for randomization

**UI Architecture**:
- Uses `TTabControl` to switch between quiz modes without visible tabs
- Mobile-friendly layout with touch-optimized button sizes
- Progress tracking with both text labels and progress bar
- Answer feedback system with color-coded rectangles

### Important Methods

**Data Module Methods**:
- `LoadStates()` - Loads all states and counties from database (udmCountyData.pas:78)
- `GetStatesList()` - Returns list of state names (udmCountyData.pas:105)
- `GetCountiesForState()` - Returns counties for specific state (udmCountyData.pas:120)
- `GetRandomCountyFromOtherStates()` - Returns random county from other states (udmCountyData.pas:138)
- `InitializeDatabase()` - Sets up SQLite database connection (udmCountyData.pas:50)

**Main Form Methods**:
- `InitializeStates()` - Loads state data from data module (ufrmCountyQuizzerMain.pas:165)
- `SetQuizMode()` - Switches between quiz modes and updates UI (ufrmCountyQuizzerMain.pas:400)
- `ShuffleCounties()` - Randomizes county order using Fisher-Yates algorithm (ufrmCountyQuizzerMain.pas:278)
- `GenerateMultipleChoice()` - Creates multiple choice options with distractors from other states (ufrmCountyQuizzerMain.pas:456)
- `CheckRecallAnswer()` - Validates user input in recall mode (ufrmCountyQuizzerMain.pas:486)

## Development Notes

### Database Setup
1. Create the SQLite database by running `counties_data.sql` script
2. Place the resulting `counties.db` file in the user's Documents folder
3. The application expects the database at: `%USERPROFILE%\Documents\counties.db` (Windows) or equivalent on other platforms

### Adding New States
To add new states:
1. Insert state record into `states` table: `INSERT INTO states (name, county_count) VALUES ('StateName', CountyCount)`
2. Insert county records into `counties` table: `INSERT INTO counties (state_id, name) VALUES ((SELECT id FROM states WHERE name = 'StateName'), 'CountyName')`
3. Restart the application - states are loaded from database on startup via the data module

### Database Schema
```sql
CREATE TABLE states (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    county_count INTEGER NOT NULL
);

CREATE TABLE counties (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    state_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    FOREIGN KEY (state_id) REFERENCES states(id)
);
```

### Quiz Mode Extensions
Each quiz mode is handled by the `SetQuizMode()` method and corresponding case statements in event handlers. The UI uses a single `TTabControl` with hidden tabs to manage different input controls for each mode.

### Mobile Compatibility
The app is designed for cross-platform deployment with mobile-friendly touch targets and responsive layout managed by `SetupMobileLayout()`.

### Dependencies
- FireDAC components for SQLite database access (included in data module)
- SQLite database file must exist before running the application
- Data module must be created before main form in project file

### Architecture Benefits
- **Separation of Concerns**: UI logic in main form, data access in data module
- **Reusability**: Data module can be used by other forms or units
- **Testability**: Database logic can be tested independently
- **Maintainability**: All database operations centralized in one location