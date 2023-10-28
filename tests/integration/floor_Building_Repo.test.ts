import { expect } from 'chai';
import { Floor } from '../../src/domain/floor'
import { Building } from '../../src/domain/building'  

describe('Integration test Floor / Building /Repositorio', () => {
  it('should create a valid Floor instance', () => {

    const buildingProps = {
        code: 'B001',
        maxWidth: 10,
        maxLength: 20,
        name: 'Sample Building',
        description: 'A test building',
    };
  
    const buildingResult = Building.create(buildingProps);
    const building = buildingResult.getValue();

    const floorProps = {
        buildingId: building.id.toString(),
        width: 10,
        length: 20,
        floorNumber: 2,
        description: "Description for floor",
        floorMap: [[1,2,3],[3,2,1]],
    }

    const floorResult = Floor.create(floorProps);
    expect(buildingResult.isSuccess).to.be.true;
    const floor = floorResult.getValue();

    expect(floor.buildingId).to.equal(floorProps.buildingId);
    expect(floor.width).to.equal(floorProps.width);
    expect(floor.length).to.equal(floorProps.length);
    expect(floor.floorMap).to.equal(floorProps.floorMap);
    expect(floor.description).to.equal(floorProps.description);
    expect(floor.floorNumber).to.equal(floorProps.floorNumber);

  });

  it('should fail to create a floor with missing required properties', () => {

    const buildingProps = {
        code: 'B001',
        maxWidth: 10,
        maxLength: 20,
        name: 'Sample Building',
        description: 'A test building',
    };
  
    const buildingResult = Building.create(buildingProps);
    const building = buildingResult.getValue();

    const floorProps = {
        buildingId: building.id.toString(),
        width: 10,
        length: 20,
        floorNumber: 2,
        floorMap: [[1,2,3],[3,2,1]],
    }

    const floorResult = Floor.create(floorProps);
    expect(floorResult.isFailure).to.be.true;
    expect(floorResult.error).to.not.be.undefined;

  });

  it('should fail to create a floor without valid builing id', () => {

    const floorProps = {
        buildingId: "ddedfcdefef",
        width: 10,
        length: 20,
        floorNumber: 2,
        floorMap: [[1,2,3],[3,2,1]],
    }

    const floorResult = Floor.create(floorProps);
    expect(floorResult.isFailure).to.be.true;
    expect(floorResult.error).to.not.be.undefined;

  });

  it('should be possible edit the floor props', () => {

    const buildingProps = {
        code: 'B001',
        maxWidth: 10,
        maxLength: 20,
        name: 'Sample Building',
        description: 'A test building',
    };
  
    const buildingResult = Building.create(buildingProps);
    const building = buildingResult.getValue();

    const floorProps = {
        buildingId: building.id.toString(),
        width: 10,
        length: 20,
        floorNumber: 2,
        floorMap: [[1,2,3],[3,2,1]],
        description: 'A test building',
    }

    const floorResult = Floor.create(floorProps);
    const floor = floorResult.getValue();

    expect(floor.width).to.equal(10);
    expect(floor.length).to.equal(20);
    expect(floor.floorNumber).to.equal(2);

    floor.width = 30;
    floor.length = 30;
    floor.floorMap = [[3,2,1],[1,2,3]];
    floor.description = "New description";

    expect(floor.width).to.equal(30);
    expect(floor.length).to.equal(30);
    expect(floor.description).to.equal('New description');
  });
})