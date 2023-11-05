import { expect } from 'chai';
import { RobotType } from '../src/domain/robotType-agg/robotType';
import TaskType from '../src/enums/taskType';

describe('RobotType', () => {
  it('should create a valid RobotType instance', () => {
    const robotTypeProps = {
      id: "X",
      designacao: 'Robot X',
      tipoTarefas: [TaskType.Task1]
    };

    const robotTypeResult = RobotType.create(robotTypeProps);

    expect(robotTypeResult.isSuccess).to.be.true;
    const robotType = robotTypeResult.getValue();
    expect(robotType.designacao).to.equal(robotTypeProps.designacao);
    expect(robotType.tipoTarefas).to.deep.equal(robotTypeProps.tipoTarefas);
  });

  it('should fail to create a RobotType with missing required properties', () => {
    const robotTypeProps = {
        id: "A",
        designacao: undefined,
        tipoTarefas: [TaskType.Task1, TaskType.Task2, TaskType.Task3]
    };

    const robotTypeResult = RobotType.create(robotTypeProps);

    expect(robotTypeResult.isFailure).to.be.true;
    expect(robotTypeResult.error).to.not.be.undefined;
  });

  it('should be possible edit the RobotType properties', () => {
    const robotTypeProps = {
        id: "B",
        designacao: 'Robot B',
        tipoTarefas: [TaskType.Task1, TaskType.Task2, TaskType.Task3]
    };

    const robotTypeResult = RobotType.create(robotTypeProps);
    const robotType = robotTypeResult.getValue();

    expect(robotType.designacao).to.equal('Robot B');
    expect(robotType.tipoTarefas).to.deep.equal([TaskType.Task1, TaskType.Task2,TaskType.Task3]);

    robotType.designacao = 'Robot B';
    robotType.tipoTarefas = [TaskType.Task1, TaskType.Task2];

    expect(robotType.designacao).to.equal('Robot B');
    expect(robotType.tipoTarefas).to.deep.equal([TaskType.Task1, TaskType.Task2]);


  });
});
