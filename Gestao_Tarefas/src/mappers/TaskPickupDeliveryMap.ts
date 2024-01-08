import {Mapper} from "../core/infra/Mapper";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {TaskPickupDelivery} from "../domain/task-agg/TaskPickupDelivery";
import ITaskPickupDeliveryDTO from "../dto/ITaskPickupDeliveryDTO";
import {Document, Model} from "mongoose";
import {ITaskPickupDeliveryPersistence} from "../dataschema/ITaskPickupDeliveryPersistence";
import ITaskSearchResponseDTO, {TaskType} from "../dto/ITaskSearchResponseDTO";

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
            pickupContact: taskPickupDelivery.pickupContact,
            taskStatus: taskPickupDelivery.taskStatus.props,
      } as ITaskPickupDeliveryDTO;
    }

    public static toDomain (taskPickupDelivery: any | Model<ITaskPickupDeliveryPersistence & Document> ): TaskPickupDelivery {

      const taskOrError = TaskPickupDelivery.create({
            description: taskPickupDelivery.description,
            pickupLocalization: taskPickupDelivery.pickupLocalization,
            deliveryLocalization: taskPickupDelivery.deliveryLocalization,
            contactNumber: taskPickupDelivery.contactNumber,
            user: taskPickupDelivery.user,
            deliveryContact: taskPickupDelivery.deliveryContact,
            pickupContact: taskPickupDelivery.pickupContact,
            taskStatus: taskPickupDelivery.taskStatus
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
        pickupContact: taskPickupDelivery.pickupContact,
        taskStatus: taskPickupDelivery.taskStatus.props,
      }
      return a;
    }

  public static toSearchResponseDTO( taskPickupDelivery: TaskPickupDelivery): ITaskSearchResponseDTO {
      return {
        id: taskPickupDelivery.id.toString(),
        description: taskPickupDelivery.description,
        pickupLocalization: taskPickupDelivery.pickupLocalization,
        deliveryLocalization: taskPickupDelivery.deliveryLocalization,
        contactNumber: taskPickupDelivery.contactNumber,
        user: taskPickupDelivery.user,
        deliveryContact: taskPickupDelivery.deliveryContact,
        pickupContact: taskPickupDelivery.pickupContact,
        taskStatus: taskPickupDelivery.taskStatus.props,
        taskType: TaskType.DELIVERY
      } as ITaskSearchResponseDTO;
  }
}
