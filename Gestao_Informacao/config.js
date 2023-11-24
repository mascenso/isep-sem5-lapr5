import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port : optional change to 4000 by JRT
   */
  port: parseInt(process.env.PORT, 10) || 4000,

  /**
   * That long string from mlab
   */
  //databaseURL: process.env.MONGODB_URI || "mongodb://127.0.0.1:27017/test",

  //Base de dados do DEI
  databaseURL: process.env.MONGODB_URI || "mongodb://mongoadmin:9e591ec9fd4cb27b85363734@vs199.dei.isep.ipp.pt:27017/admin",
  
  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  corsOrigin: process.env.CORS_ORIGIN, // || "https://isep-sem5pi-079.vercel.app",

  controllers: {
    role: {
      name: "RoleController",
      path: "../controllers/roleController"
    },
    robot: {
      name: "RobotController",
      path: "../controllers/robotController"
    },
    robotType: {
      name: "RobotTypeController",
      path: "../controllers/robotTypeController"
    },
    building:  {
      name: "BuildingController",
      path: "../controllers/buildingController"
    },
    floor:  {
      name: "FloorController",
      path: "../controllers/floorController"
    },
    bridge:  {
      name: "BridgeController",
      path: "../controllers/bridgeController"
    },
    elevator:  {
      name: "ElevatorController",
      path: "../controllers/elevatorController"
    },
    room:  {
      name: "RoomController",
      path: "../controllers/roomController"
    }
  },

  repos: {
    role: {
      name: "RoleRepo",
      path: "../repos/roleRepo"
    },
    robot: {
      name: "RobotRepo",
      path: "../repos/robotRepo"
    },
    robotType: {
      name: "RobotTypeRepo",
      path: "../repos/robotTypeRepo"
    },
    user: {
      name: "UserRepo",
      path: "../repos/userRepo"
    },
    building: {
      name: "BuildingRepo",
      path: "../repos/buildingRepo"
    },
    floor: {
      name: "FloorRepo",
      path: "../repos/floorRepo"
    },
    bridge: {
      name: "BridgeRepo",
      path: "../repos/bridgeRepo"
    },
    elevator: {
      name: "ElevatorRepo",
      path: "../repos/elevatorRepo"
    },
    room: {
      name: "RoomRepo",
      path: "../repos/roomRepo"
    }
  },

  services: {
    role: {
      name: "RoleService",
      path: "../services/roleService"
    },
    robot: {
      name: "RobotService",
      path: "../services/robotService"
    },
    robotType: {
      name: "RobotTypeService",
      path: "../services/robotTypeService"
    },
    building: {
      name: "BuildingService",
      path: "../services/buildingService"
    },
    floor: {
      name: "FloorService",
      path: "../services/floorService"
    },
    bridge: {
      name: "BridgeService",
      path: "../services/bridgeService"
    },
    elevator: {
      name: "ElevatorService",
      path: "../services/elevatorService"
    },
    room: {
      name: "RoomService",
      path: "../services/roomService"
    }
  },
};
