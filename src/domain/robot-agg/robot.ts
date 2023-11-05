import { SerialNumber } from "../serialNumber";
import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { RobotId } from "./robotId";
import { Result } from "../../core/logic/Result";
import { Guard } from "../../core/logic/Guard";

/*
-- código identificativo, obrigatório, alfanumerico, max 30 caracteres, único no sistema
-- nickname, obrigatório, obrigatório, alfanumerico, max 30 caracteres, único no sistema
-- tipo de robot, obrigatório
-- número de série, obrigatório, alfanumerico, max 50 caracteres, único para um dado tipo de robot -  único para cada robot da mesma marca
-- descrição, opcional, alfanumerico, max. 250 caracteres

    "viatura com matricula AA-00-01 do tipo 1"
 */
export interface RobotProps {
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}

export class Robot extends AggregateRoot<RobotProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get serialNumber() {
        return this.props.serialNumber;
    }

    get nickName() {
        return this.props.nickName;
    }

    get inhibited() {
        return this.props.inhibited;
    }

    get description() {
        return this.props.description;
    }

    get robotType() {
        return this.props.robotType;
    }

    set serialNumber(serialNumber: string) {
        const sn = SerialNumber.create(serialNumber);
        //this.props.serialNumber = sn.getValue();
        this.props.serialNumber = serialNumber;
    }

    set nickName(nickName: string) {
        this.props.nickName = nickName;
    }

    set description(description: string) {
        this.props.description = description;
    }

    set inhibited(inhibited: boolean) {
        this.props.inhibited = inhibited;
    }

    set robotType(robotType: string) {
        this.props.robotType = robotType;
    }


    private constructor(props: RobotProps, id?: RobotId) {
        super(props, id);
    }

    public static create(props: RobotProps, id?: UniqueEntityID): Result<Robot> {

          const guardedProps = [

              { argument: props.serialNumber, argumentName: 'serialNumber' },
              { argument: props.nickName, argumentName: 'nickName' },
              { argument: props.inhibited, argumentName: 'inhibited' }

          ];

          const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

          if (!guardResult.succeeded) {
              return Result.fail<Robot>(guardResult.message)
          }
          else {
              const robot = new Robot({
                  ...props
              }, id);

              return Result.ok<Robot>(robot);
          }
    }
}
