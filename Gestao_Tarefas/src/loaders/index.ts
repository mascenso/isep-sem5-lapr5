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
    schema: '../persistence/schemas/taskSchema',
  };

  const taskController = {
    name: config.controllers.task.name,
    path: config.controllers.task.path
  }

  const taskRepo = {
    name: config.repos.task.name,
    path: config.repos.task.path
  }

  const taskService = {
    name: config.services.task.name,
    path: config.services.task.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      taskSchema
    ],
    controllers: [
      taskController
    ],
    repos: [
      taskRepo
    ],
    services: [
      taskService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
