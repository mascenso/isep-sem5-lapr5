import { expect } from 'chai';
import {Building} from '../src/domain/building'  // Adjust the path to the Building class

describe('Building', () => {
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
});
