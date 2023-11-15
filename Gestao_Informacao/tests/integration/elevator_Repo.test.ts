import { expect } from 'chai';
import {Elevator} from '../../src/domain/elevator-agg/elevator'
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';

describe('Elevator', () => {
  it('should create a valid Elevator instance', () => {
    const elevatorProps = {
      code: 'Elev1',
      floorList: ['F1', 'F2'], 
      buildingId: 'B1',
    };

    // Cria uma instância de Elevator
    const elevator = Elevator.create(elevatorProps);

    // Verifica se a instância foi criada com sucesso
    expect(elevator.isSuccess).to.be.true;

    // Acessa as propriedades do elevador
    const elevatorInstance = elevator.getValue();
    expect(elevatorInstance.code).to.equal(elevatorProps.code);
    expect(elevatorInstance.floorList).to.deep.equal(elevatorProps.floorList);
    expect(elevatorInstance.buildingId).to.equal(elevatorProps.buildingId);
  });


  it('should be possible edit the elevator props', () => {
    const elevatorProps = {
      code: 'Elev1',
      floorList: ['F1', 'F2'],
      buildingId: 'B1',
    };

    const elevatorResult = Elevator.create(elevatorProps);
    const elevator = elevatorResult.getValue();

    expect(elevator.code).to.equal(elevatorProps.code);
    expect(elevator.floorList).to.deep.equal(elevatorProps.floorList);
    expect(elevator.buildingId).to.equal(elevatorProps.buildingId);

    elevator.code = 'Elev2';
    elevator.floorList= ['F1', 'F2'];

    expect(elevator.code).to.equal('Elev2');
  });

});
