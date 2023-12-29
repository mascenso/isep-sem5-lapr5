import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const buildingSchema = {
    // compare with the approach followed in repos and services
    name: 'buildingSchema',
    schema: '../persistence/schemas/buildingSchema',
  };

  const floorSchema = {
    // compare with the approach followed in repos and services
    name: 'floorSchema',
    schema: '../persistence/schemas/floorSchema',
  };

  const robotSchema = {
    // compare with the approach followed in repos and services
    name: 'robotSchema',
    schema: '../persistence/schemas/robotSchema',
  };

  const robotTypeSchema = {
    // compare with the approach followed in repos and services
    name: 'robotTypeSchema',
    schema: '../persistence/schemas/robotTypeSchema',
  };
  const bridgeSchema = {
    // compare with the approach followed in repos and services
    name: 'bridgeSchema',
    schema: '../persistence/schemas/bridgeSchema',
  }

  const elevatorSchema = {
    // compare with the approach followed in repos and services
    name: 'elevatorSchema',
    schema: '../persistence/schemas/elevatorSchema',
  };

  const roomSchema = {
    // compare with the approach followed in repos and services
    name: 'roomSchema',
    schema: '../persistence/schemas/roomSchema',
  };

  const robotController = {
    name: config.controllers.robot.name,
    path: config.controllers.robot.path
  };

  const robotTypeController = {
    name: config.controllers.robotType.name,
    path: config.controllers.robotType.path
  };

  const buildingController = {
    name: config.controllers.building.name,
    path: config.controllers.building.path
  }

  const floorController = {
    name: config.controllers.floor.name,
    path: config.controllers.floor.path
  }

  const bridgeController = {
    name: config.controllers.bridge.name,
    path: config.controllers.bridge.path
  }

  const elevatorController = {
    name: config.controllers.elevator.name,
    path: config.controllers.elevator.path
  }

  const roomController = {
    name: config.controllers.room.name,
    path: config.controllers.room.path
  }

  const robotRepo = {
    name: config.repos.robot.name,
    path: config.repos.robot.path
  };

  const robotTypeRepo = {
    name: config.repos.robotType.name,
    path: config.repos.robotType.path
  };

  const buildingRepo = {
    name: config.repos.building.name,
    path: config.repos.building.path
  }

  const floorRepo = {
    name: config.repos.floor.name,
    path: config.repos.floor.path
  }

  const bridgeRepo = {
    name: config.repos.bridge.name,
    path: config.repos.bridge.path
  }

  const elevatorRepo = {
    name: config.repos.elevator.name,
    path: config.repos.elevator.path
  }

  const roomRepo = {
    name: config.repos.room.name,
    path: config.repos.room.path
  }

  const robotService = {
    name: config.services.robot.name,
    path: config.services.robot.path
  }

  const robotTypeService = {
    name: config.services.robotType.name,
    path: config.services.robotType.path
  }

  const buildingService = {
    name: config.services.building.name,
    path: config.services.building.path
  }

  const floorService = {
    name: config.services.floor.name,
    path: config.services.floor.path
  }

  const bridgeService = {
    name: config.services.bridge.name,
    path: config.services.bridge.path
  }

  const elevatorService = {
    name: config.services.elevator.name,
    path: config.services.elevator.path
  }

  const roomService = {
    name: config.services.room.name,
    path: config.services.room.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      buildingSchema,
      robotSchema,
      robotTypeSchema,
      floorSchema,
      elevatorSchema,
      bridgeSchema,
      roomSchema
    ],
    controllers: [
      buildingController,
      robotController,
      robotTypeController,
      floorController,
      elevatorController,
      bridgeController,
      roomController
    ],
    repos: [
      buildingRepo,
      robotRepo,
      robotTypeRepo,
      floorRepo,
      elevatorRepo,
      bridgeRepo,
      roomRepo
    ],
    services: [
      buildingService,
      robotService,
      robotTypeService,
      floorService,
      bridgeService,
      elevatorService,
      roomService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
