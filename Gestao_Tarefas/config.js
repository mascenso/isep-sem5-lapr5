import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';
//Production
//process.env.NODE_ENV = process.env.NODE_ENV || 'production';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port : optional change to 4000 by JRT
   */
  port: parseInt(process.env.PORT, 10) || 3000,

  /**
   * That long string from mlab
   */
  databaseURL: process.env.MONGODB_URI || "mongodb://127.0.0.1:27017/tasks",
  //databaseURL: process.env.MONGODB_URI || "mongodb://***REMOVED***@vsgate-s1.dei.isep.ipp.pt:10199/tasksDB",

  //dummy database para teste e2e
  //databaseURL: process.env.MONGODB_URI || "mongodb://***REMOVED***@vsgate-s1.dei.isep.ipp.pt:10199/dummyDB",
  /**
   *
   */

  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

  apiUrlPROLOG: process.env.NODE_ENV === 'production'
    ? 'https://robdronego.westeurope.cloudapp.azure.com'
    : 'http://127.0.0.1:8081',

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

  controllers: {
    task: {
      name: "TaskController",
      path: "../controllers/taskController"
    },
    route: {
      name: "RouteController",
      path: "../controllers/routeController"
    }
  },

  repos: {
    task: {
      name: "TaskRepo",
      path: "../repos/taskRepo"
    },
    taskVigilance: {
      name: "TaskVigilanceRepo",
      path: "../repos/TaskVigilanceRepo"
    },
    taskPickupDelivery: {
      name: "TaskPickupDeliveryRepo",
      path: "../repos/TaskPickupDeliveryRepo"
    },

  },

  services: {
    task: {
      name: "TaskService",
      path: "../services/taskService"
    },
    route: {
      name: "RouteService",
      path: "../services/routeService"
    }
  },
};
