import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');


  const taskSchema = {
    // compare with the approach followed in repos and services
    name: 'taskSchema',
    schema: '../persistence/schemas/TaskSchema',
  };
  const taskVigilanceSchema = {
    // compare with the approach followed in repos and services
    name: 'taskVigilanceSchema',
    schema: '../persistence/schemas/TaskVigilanceSchema',
  };
  const taskPickupDeliverySchema = {
    // compare with the approach followed in repos and services
    name: 'taskPickupDeliverySchema',
    schema: '../persistence/schemas/TaskPickupDeliverySchema',
  };

  const taskController = {
    name: config.controllers.task.name,
    path: config.controllers.task.path
  }
  const routeController = {
    name: config.controllers.route.name,
    path: config.controllers.route.path
  }

  const taskRepo = {
    name: config.repos.task.name,
    path: config.repos.task.path
  }
  const taskVigilanceRepo = {
    name: config.repos.taskVigilance.name,
    path: config.repos.taskVigilance.path
  }
  const taskPickupDeliveryRepo = {
    name: config.repos.taskPickupDelivery.name,
    path: config.repos.taskPickupDelivery.path
  }

  const taskService = {
    name: config.services.task.name,
    path: config.services.task.path
  }

  const routeService = {
    name: config.services.route.name,
    path: config.services.route.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      taskSchema,
      taskVigilanceSchema,
      taskPickupDeliverySchema
    ],
    controllers: [
      taskController,
      routeController
    ],
    repos: [
      taskRepo,
      taskVigilanceRepo,
      taskPickupDeliveryRepo
    ],
    services: [
      taskService,
      routeService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
