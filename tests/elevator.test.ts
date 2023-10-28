import { expect } from 'chai';
import {Elevator} from '../src/domain/elevator'  
import { UniqueEntityID } from '../src/core/domain/UniqueEntityID';

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
/*
  it('should fail to create an Elevator with missing required properties', () => {
    const elevatorProps = {
        code: 'Elev1',
        floorId: 'F1', 
        coordX1: 2,
        coordY1: 2,
        coordX2: 2
      };
    
      // Tenta criar uma instância de Elevator
     const elevator = Elevator.create(elevatorProps);

    // Verifica se a criação falhou
    expect(elevator.isFailure).toBe(true);
  });
*/
});
