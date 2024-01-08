import { Service, Inject } from 'typedi';

import { TaskVigilanceId } from "../domain/task-agg/taskVigilanceId";
import { TaskVigilanceMap } from "../mappers/TaskVigilanceMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { ITaskVigilancePersistence } from '../dataschema/ITaskVigilancePersistence';
import { TaskVigilance } from '../domain/task-agg/TaskVigilance';
import ITaskVigilanceRepo from '../services/IRepos/ITaskVigilanceRepo';
import {TaskStatus} from "../domain/task-agg/TaskStatus";

@Service()
export default class TaskVigilanceRepo implements ITaskVigilanceRepo {
  private models: any;

  constructor(
    @Inject('taskVigilanceSchema') private taskVigilanceSchema : Model<ITaskVigilancePersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(taskVigilance: TaskVigilance): Promise<boolean> {

    const idX = taskVigilance.id instanceof TaskVigilanceId ? (<TaskVigilanceId>taskVigilance.id).toValue() : taskVigilance.id;

    const query = { domainId: idX};
    const taskDocument = await this.taskVigilanceSchema.findOne( query as FilterQuery<ITaskVigilancePersistence & Document>);

    return !!taskDocument === true;
  }

  public async save (taskVigilance: TaskVigilance): Promise<TaskVigilance> {
    const query = { domainId: taskVigilance.id.toString()};

    const taskDocument = await this.taskVigilanceSchema.findOne( query );

    try {
      if (taskDocument === null ) {
        const rawTask: any = TaskVigilanceMap.toPersistence(taskVigilance);

        const taskCreated = await this.taskVigilanceSchema.create(rawTask);

        return TaskVigilanceMap.toDomain(taskCreated);
      } else {
        const updateFields = ['taskStatus']

        for (const field of updateFields) {
          if (taskVigilance[field] !== undefined) {
            taskDocument[field] = taskVigilance[field].props;
          }
        }

        await taskDocument.save();
        return taskVigilance;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByDomainId (taskId: TaskVigilanceId | string): Promise<TaskVigilance> {
    const query = { domainId: taskId};
    const taskRecord = await this.taskVigilanceSchema.findOne( query as FilterQuery<ITaskVigilancePersistence & Document> );

    if( taskRecord != null) {
      return TaskVigilanceMap.toDomain(taskRecord);
    }
    else
      return null;
  }

  public async findAll(): Promise<TaskVigilance[]> {
    const tasks = await this.taskVigilanceSchema.find();
    return tasks.map((task) => TaskVigilanceMap.toDomain(task));
  }


  public async findByUserEmail(email: string): Promise<TaskVigilance[]> {
    const query = {user: {userEmail: email}};
    const tasks = await this.taskVigilanceSchema.find(query);
    return tasks.map(task => TaskVigilanceMap.toDomain(task));
  }

  public async findByTaskStatus(status: TaskStatus): Promise<TaskVigilance[]> {
    const query = this.mapToQuery(status);
    const tasks = await this.taskVigilanceSchema.find(query);
    return tasks.map(task => TaskVigilanceMap.toDomain(task));
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

