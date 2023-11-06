import { expect } from 'chai';
import {Elevator} from '../../src/domain/elevator-agg/elevator'
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';

describe('Elevator', () => {
  it('should create a valid Elevator instance', () => {
    const elevatorProps = {
      code: 'Elev1',
      floorId: 'F1',
      coordX1: 2,
      coordY1: 2,
      coordX2: 2,
      coordY2: 2,
    };

    // Cria uma instância de Elevator
    const elevator = Elevator.create(elevatorProps);

    // Verifica se a instância foi criada com sucesso
    expect(elevator.isSuccess).to.be.true;

    // Acessa as propriedades do elevador
    const elevatorInstance = elevator.getValue();
    expect(elevatorInstance.code).to.equal(elevatorProps.code);
    expect(elevatorInstance.floorId).to.equal(elevatorProps.floorId);
    expect(elevatorInstance.coordX1).to.equal(elevatorProps.coordX1);
    expect(elevatorInstance.coordY1).to.equal(elevatorProps.coordY1);
    expect(elevatorInstance.coordX2).to.equal(elevatorProps.coordX2);
    expect(elevatorInstance.coordY2).to.equal(elevatorProps.coordY2);
  });


  it('should be possible edit the elevator props', () => {
    const elevatorProps = {
      code: 'Elev1',
      floorId: 'F1',
      coordX1: 2,
      coordY1: 2,
      coordX2: 2,
      coordY2: 2,
    };

    const elevatorResult = Elevator.create(elevatorProps);
    const elevator = elevatorResult.getValue();

    expect(elevator.code).to.equal(elevatorProps.code);
    expect(elevator.floorId).to.equal(elevatorProps.floorId);
    expect(elevator.coordX1).to.equal(elevatorProps.coordX1);
    expect(elevator.coordY1).to.equal(elevatorProps.coordY1);
    expect(elevator.coordX2).to.equal(elevatorProps.coordX2);
    expect(elevator.coordY2).to.equal(elevatorProps.coordY2);

    elevator.code = 'Elev2';
    elevator.coordX1 = 1;
    elevator.coordX2 = 1;
    elevator.coordY1 = 1;
    elevator.coordY2 = 1;

    expect(elevator.code).to.equal('Elev2');
    expect(elevator.coordX1).to.equal(1);
    expect(elevator.coordY1).to.equal(1);
    expect(elevator.coordX2).to.equal(1);
    expect(elevator.coordY2).to.equal(1);
  });

});
