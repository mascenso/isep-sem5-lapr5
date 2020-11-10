
import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export class RoleId extends Entity<any> {

  get id (): UniqueEntityID {
    return this._id;
  }

  public static create (id?: UniqueEntityID): RoleId {
    return new RoleId(id);
  }
}