import { expect } from 'chai';
import {Building} from '../../src/domain/building'  

describe('Integration test Building / Repositorio', () => {
  it('should create a valid Building instance', () => {
    const buildingProps = {
      code: 'B001',
      maxWidth: 10,
      maxLength: 20,
      name: 'Sample Building',
      description: 'A test building',
    };

    const buildingResult = Building.create(buildingProps);

    expect(buildingResult.isSuccess).to.be.true;
    const building = buildingResult.getValue();
    expect(building.code).to.equal(buildingProps.code);
    expect(building.maxWidth).to.equal(buildingProps.maxWidth);
    expect(building.maxLength).to.equal(buildingProps.maxLength);
    expect(building.name).to.equal(buildingProps.name);
    expect(building.description).to.equal(buildingProps.description);
  });

  it('should fail to create a Building with missing required properties', () => {
    const buildingProps = {
      code: 'B002',
      maxWidth: 15,
      maxLength:15
    };

    const buildingResult = Building.create(buildingProps);

    expect(buildingResult.isFailure).to.be.true;
    expect(buildingResult.error).to.not.be.undefined;
  });

  it('should be possible edit the building props', () => {
    const buildingProps = {
      code: 'B001',
      maxWidth: 10,
      maxLength: 20,
      name: 'Sample Building',
      description: 'A test building',
    };

    const buildingResult = Building.create(buildingProps);
    const building = buildingResult.getValue();

    expect(building.maxWidth).to.equal(10);
    expect(building.maxLength).to.equal(20);
    expect(building.code).to.equal("B001");
    expect(building.description).to.equal('A test building');
    expect(building.name).to.equal('Sample Building');

    building.maxLength = 30;
    building.maxWidth = 30;
    building.code = "B002";
    building.name = "New building";
    building.description = "New description";

    expect(building.maxWidth).to.equal(30);
    expect(building.maxLength).to.equal(30);
    expect(building.code).to.equal("B002");
    expect(building.description).to.equal('New description');
    expect(building.name).to.equal('New building');
  });
});

