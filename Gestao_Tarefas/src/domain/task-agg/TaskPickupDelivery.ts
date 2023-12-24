import {AggregateRoot} from "../../core/domain/AggregateRoot";
import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Result} from "../../core/logic/Result";
import {Guard} from "../../core/logic/Guard";

interface TaskPickupDeliveryProps {
    description: string;
    pickupLocalization: {
      buildingId:String;
      floor:object;
      room: number[];
    };
    deliveryLocalization:{
      buildingId:String;
      floor:object;
      room: number[];
    };
    contactNumber:number;
    user:object;
    deliveryContact:{
      name:String;
      contactNumber:number;
    };
    pickupContact:{
      name:String;
      contactNumber:number;
    };
    approved:boolean;
    pending:boolean;
    planned:boolean;
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
      { argument: props.approved, argumentName: 'approved' },
      { argument: props.pending, argumentName: 'pending' },
      { argument: props.planned, argumentName: 'planned' },
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
  public get approved() : boolean {
    return this.props.approved;
  }
  public get pending() : boolean {
    return this.props.pending;
  }
  public get planned() : boolean {
    return this.props.planned;
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

  set user ( value: object) {
    this.props.user = value;
  }

  set deliveryContact ( value: any) {
    this.props.deliveryContact = value;
  }

  set pickupContact ( value: any) {
    this.props.pickupContact = value;
  }
  set approved ( value: boolean) {
    this.props.approved = value;
  }
  set pending ( value: boolean) {
    this.props.pending = value;
  }
  set planned ( value: boolean) {
    this.props.planned = value;
  }
}
