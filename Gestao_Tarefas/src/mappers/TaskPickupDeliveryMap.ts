import { Mapper } from "../core/infra/Mapper";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import {TaskPickupDelivery} from "../domain/task-agg/taskPickupDelivery";
import ITaskPickupDeliveryDTO from "../dto/ITaskPickupDeliveryDTO";
import {Document, Model} from "mongoose";
import {ITaskPickupDeliveryPersistence} from "../dataschema/ITaskPickupDeliveryPersistence";

export class TaskPickupDeliveryMap extends Mapper<TaskPickupDelivery> {

    public static toDTO( taskPickupDelivery: TaskPickupDelivery): ITaskPickupDeliveryDTO {
  
      return {
            id: taskPickupDelivery.id.toString(),
            description: taskPickupDelivery.description,
            pickupLocalization: taskPickupDelivery.pickupLocalization,
            deliveryLocalization: taskPickupDelivery.deliveryLocalization,
            contactNumber: taskPickupDelivery.contactNumber,
            user: taskPickupDelivery.user,
            deliveryContact: taskPickupDelivery.deliveryContact,
            pickupDelivery: taskPickupDelivery.pickupDelivery
      } as ITaskPickupDeliveryDTO;
    }
  
    public static toDomain (taskPickupDelivery: any | Model<ITaskPickupDeliveryPersistence & Document> ): TaskPickupDelivery {
  
      const taskOrError = taskPickupDelivery.create({
            description: taskPickupDelivery.description,
            pickupLocalization: taskPickupDelivery.pickupLocalization,
            deliveryLocalization: taskPickupDelivery.deliveryLocalization,
            contactNumber: taskPickupDelivery.contactNumber,
            user: taskPickupDelivery.user,
            deliveryContact: taskPickupDelivery.deliveryContact,
            pickupDelivery: taskPickupDelivery.pickupDelivery
        },
        new UniqueEntityID(taskPickupDelivery.domainId)
      );
  
      taskOrError.isFailure ? console.log(taskOrError.error) : '';
  
      return taskOrError.isSuccess ? taskOrError.getValue() : null;
    }
  
    public static toPersistence (taskPickupDelivery: TaskPickupDelivery): any {
      const a = {
        domainId: taskPickupDelivery.id.toString(),
        description: taskPickupDelivery.description,
        pickupLocalization: taskPickupDelivery.pickupLocalization,
        deliveryLocalization: taskPickupDelivery.deliveryLocalization,
        contactNumber: taskPickupDelivery.contactNumber,
        user: taskPickupDelivery.user,
        deliveryContact: taskPickupDelivery.deliveryContact,
        pickupDelivery: taskPickupDelivery.pickupDelivery
      }
      return a;
    }
  }
  