import { expect } from 'chai';
import {Building} from "../src/domain/building-agg/building";

describe('Building', () => {
  it('given valid properties creates a valid Building instance', () => {
    const validProps = {
      code: 'B123',
      maxWidth: 10,
      maxLength: 20,
      name: 'Test Building',
      description: 'Test Description',
    };

    const building = Building.create(validProps);

    expect(building.isSuccess).to.be.true;
    expect(building.getValue()).to.be.an.instanceOf(Building);
  });

  it('given missing required code fails to create Building instance', () => {
    const invalidProps = {
      code: undefined,
      maxWidth: 10,
      maxLength: 20,
    };

    const building = Building.create(invalidProps);

    expect(building.isFailure).to.be.true;
    expect(building.error).to.be.equal('code is null or undefined');
  });

  it('given missing required maxWidth fails to create Building instance', () => {
    const invalidProps = {
      code: 'B123',
      maxWidth: undefined,
      maxLength: 20,
    };

    const building = Building.create(invalidProps);

    expect(building.isFailure).to.be.true;
    expect(building.error).to.be.equal('maxWidth is null or undefined');
  });
});
