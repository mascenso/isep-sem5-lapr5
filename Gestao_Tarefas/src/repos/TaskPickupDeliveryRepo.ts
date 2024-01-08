import { Service, Inject } from 'typedi';

import ITaskPickupDeliveryRepo from "../services/IRepos/ITaskPickupDeliveryRepo";

import { TaskPickupDeliveryMap } from "../mappers/TaskPickupDeliveryMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { ITaskPickupDeliveryPersistence } from '../dataschema/ITaskPickupDeliveryPersistence';
import { TaskPickupDelivery } from '../domain/task-agg/TaskPickupDelivery';
import { TaskPickupDeliveryId } from '../domain/task-agg/TaskPickupDeliveryId';
import {TaskStatus} from "../domain/task-agg/TaskStatus";

@Service()
export default class TaskPickupDeliveryRepo implements ITaskPickupDeliveryRepo {
  private models: any;

  constructor(
    @Inject('taskPickupDeliverySchema') private taskPickupDeliverySchema: Model<ITaskPickupDeliveryPersistence & Document>,
  ) { }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async exists(taskPickupDelivery: TaskPickupDelivery): Promise<boolean> {

    const idX = taskPickupDelivery.id instanceof TaskPickupDeliveryId ? (<TaskPickupDeliveryId>taskPickupDelivery.id).toValue() : taskPickupDelivery.id;

    const query = { domainId: idX };
    const taskDocument = await this.taskPickupDeliverySchema.findOne(query as FilterQuery<ITaskPickupDeliveryPersistence & Document>);

    return !!taskDocument === true;
  }

  public async save(taskPickupDelivery: TaskPickupDelivery): Promise<TaskPickupDelivery> {
    const query = { domainId: taskPickupDelivery.id.toString() };

    const taskDocument = await this.taskPickupDeliverySchema.findOne(query);

    try {
      if (taskDocument === null) {
        const rawTask: any = TaskPickupDeliveryMap.toPersistence(taskPickupDelivery);

        const taskCreated = await this.taskPickupDeliverySchema.create(rawTask);

        return TaskPickupDeliveryMap.toDomain(taskCreated);
      } else {
        taskDocument.id = taskPickupDelivery.id;
        await taskDocument.save();

        return taskPickupDelivery;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId(taskId: TaskPickupDeliveryId | string): Promise<TaskPickupDelivery> {
    const query = { domainId: taskId };
    const taskRecord = await this.taskPickupDeliverySchema.findOne(query as FilterQuery<ITaskPickupDeliveryPersistence & Document>);

    if (taskRecord != null) {
      return TaskPickupDeliveryMap.toDomain(taskRecord);
    }
    else
      return null;
  }

  public async findAll(): Promise<TaskPickupDelivery[]> {
    const tasks = await this.taskPickupDeliverySchema.find();
    return tasks.map((task) => TaskPickupDeliveryMap.toDomain(task));
  }

  public async findByUserEmail(email: string): Promise<TaskPickupDelivery[]> {
    const query = {"user.userEmail": email};
    const tasks = await this.taskPickupDeliverySchema.find(query);
    return tasks.map(task => TaskPickupDeliveryMap.toDomain(task));
  }

  public async findByTaskStatus(status: TaskStatus): Promise<TaskPickupDelivery[]> {
    const query = this.mapToQuery(status);
    const tasks = await this.taskPickupDeliverySchema.find(query);
    return tasks.map(task => TaskPickupDeliveryMap.toDomain(task));
  }

  private mapToQuery(status: TaskStatus): any {
    let query: any;
    switch (status) {
      case TaskStatus.APPROVED:
        query = {
          $and: [
            {'taskStatus.approved': true},
            {'taskStatus.planned': false},
            {'taskStatus.pending':false}
          ]
        };
        break;

      case TaskStatus.PENDING:
        query = {
          $and: [
            {'taskStatus.approved': false},
            {'taskStatus.planned': false},
            {'taskStatus.pending':true}
          ]
        };
        break;

      case TaskStatus.PLANNED:
        query = {
          $and: [
            {'taskStatus.approved': true},
            {'taskStatus.planned': true},
            {'taskStatus.pending':false}
          ]
        }
        break;

      default:
        query = {};
    }
    return query;
  }

}

