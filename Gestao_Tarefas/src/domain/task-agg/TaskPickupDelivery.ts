import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";
import { User } from "./user";
import { LocationRoom } from "./locationRoom";
import { TaskStatusVO } from "./taskStatusVO";

interface TaskPickupDeliveryProps {
    description: string;
    user: User;
    taskStatus: TaskStatusVO;

    pickupLocalization: LocationRoom;
    deliveryLocalization:LocationRoom;
    contactNumber:number;
    deliveryContact: User;

    pickupContact: User;
}

export class TaskPickupDelivery extends AggregateRoot<TaskPickupDeliveryProps> {

  private constructor (props: TaskPickupDeliveryProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: TaskPickupDeliveryProps, id?: UniqueEntityID): Result<TaskPickupDelivery> {

    const guardedProps = [
      { argument: props.description, argumentName: 'description' },
      { argument: props.pickupLocalization, argumentName: 'pickupLocalization' },
      { argument: props.deliveryLocalization, argumentName: 'deliveryLocalization' },
      { argument: props.contactNumber, argumentName: 'contactNumber' },
      { argument: props.user, argumentName: 'user' },
      { argument: props.deliveryContact, argumentName: 'deliveryContact' },
      { argument: props.pickupContact, argumentName: 'pickupContact' },
      { argument: props.taskStatus, argumentName: 'taskStatus' },
    ];


    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<TaskPickupDelivery>(guardResult.message)
    }
    else {
      const taskPickup = new TaskPickupDelivery({
        ...props
      }, id);

      return Result.ok<TaskPickupDelivery>(taskPickup);
    }
  }

  get id (): UniqueEntityID {
    return this._id;
  }

  public get description() : string {
    return this.props.description;
  }
  public get pickupLocalization() : object {
    return this.props.pickupLocalization;
  }

  public get deliveryLocalization() : object {
    return this.props.deliveryLocalization;
  }

  public get contactNumber() : number {
    return this.props.contactNumber;
  }

  public get user() : object {
    return this.props.user;
  }

  public get deliveryContact() : object {
    return this.props.deliveryContact;
  }

  public get pickupContact() : object {
    return this.props.pickupContact;
  }
  public get taskStatus() : TaskStatusVO {
    return this.props.taskStatus;
  }




  set description ( value: string) {
    this.props.description = value;
  }

  set pickupLocalization ( value: any) {
    this.props.pickupLocalization = value;
  }

  set deliveryLocalization ( value: any) {
    this.props.deliveryLocalization = value;
  }

  set contactNumber ( value: number) {
    this.props.contactNumber = value;
  }

  set user ( value: any) {
    this.props.user = value;
  }

  set deliveryContact ( value: any) {
    this.props.deliveryContact = value;
  }

  set pickupContact ( value: any) {
    this.props.pickupContact = value;
  }
  set taskStatus ( value: TaskStatusVO) {
    this.props.taskStatus = value;
  }
}
