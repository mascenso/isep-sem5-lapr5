import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { RobotTypeId } from "./robotTypeId";
import IRobotTypeDTO from "../../dto/IRobotTypeDTO";
import TaskType from '../../enums/taskType';

interface RobotTypeProps {
  designacao: string;
  tipoTarefas: TaskType[];
}


/**
 * Tipo B: Robot marca W modelo Z com capacidade de executar tarefas de vigilância e pickeup&delivery
 */
/*
tipo de robot: obrigatório, alfanum+ericos, maximo 25 caracteres
marca: obrigatório, maximo 50 caracteres
modelo: obrigatório, máximo 100 caracteres
*/
"o \"tipo de robot\" é um código identificativo desse tipo de robots."

export class RobotType extends AggregateRoot<RobotTypeProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get robotTypeId (): RobotTypeId {
    return new RobotTypeId(this.robotTypeId.toValue());
  }

  get designacao (): string {
    return this.props.designacao;
  }

  get tipoTarefas (): TaskType[] {
    return this.props.tipoTarefas;
  }

  set designacao ( value: string) {
    this.props.designacao = value;
  }

  set tipoTarefas ( value: TaskType[]) {
    this.props.tipoTarefas = value;
  }

  private constructor (props: RobotTypeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (robotTypeDTO: IRobotTypeDTO, id?: UniqueEntityID): Result<RobotType> {
    const designacao = robotTypeDTO.designacao;
    const tipoTarefas = robotTypeDTO.tipoTarefas;

    if (!!designacao === false || designacao.length === 0) {
      return Result.fail<RobotType>('Must provide a robot type designation.')
    } else {
      const robotType = new RobotType({ designacao: designacao, tipoTarefas: tipoTarefas }, id);
      return Result.ok<RobotType>( robotType )
    }
  }
}
